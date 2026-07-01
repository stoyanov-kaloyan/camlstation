#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <algorithm>
#include <array>
#include <atomic>
#include <cstdint>
#include <cstdio>
#include <memory>
#include <mutex>
#include <queue>
#include <stdexcept>
#include <string>
#include <thread>
#include <vector>

#include <SDL3/SDL.h>

class Renderer
{
public:
  Renderer()
  {
    init_sdl();
    create_window();
    create_renderer();
    create_vram_texture();
    std::printf("Renderer initialized\n");
    worker_thread = std::thread(&Renderer::thread_main, this);
  }

  ~Renderer()
  {
    shutdown();
  }

  void submit_command(int command)
  {
    std::lock_guard<std::mutex> lock(queue_mutex);
    command_queue.push(command);
  }

  void submit_gp1_command(int command)
  {
    std::lock_guard<std::mutex> lock(queue_mutex);
    gp1_command_queue.push(command);
  }

  void shutdown()
  {
    stop_requested.store(true);
    if (worker_thread.joinable())
    {
      worker_thread.join();
    }
  }

  bool should_close() const
  {
    return close_requested.load();
  }

private:
  static constexpr int VRAM_WIDTH = 1024;
  static constexpr int VRAM_HEIGHT = 512;
  const int WIDTH = 800;
  const int HEIGHT = 600;

  struct Gp0State
  {
    std::uint32_t first_word = 0;
    int words_expected = 0;
    int args_received = 0;
    std::array<std::uint32_t, 16> args{};
    bool image_load_active = false;
    int image_x = 0;
    int image_y = 0;
    int image_w = 0;
    int image_h = 0;
    int image_cur_x = 0;
    int image_cur_y = 0;
    int image_words_remaining = 0;
  };

  std::queue<int> command_queue;
  std::queue<int> gp1_command_queue;
  std::mutex queue_mutex;
  std::atomic_bool close_requested{false};
  std::atomic_bool stop_requested{false};
  std::thread worker_thread;

  SDL_Window *window = nullptr;
  SDL_Renderer *renderer = nullptr;
  SDL_Texture *vram_texture = nullptr;
  Uint32 window_id = 0;
  std::vector<std::uint16_t> vram = std::vector<std::uint16_t>(static_cast<std::size_t>(VRAM_WIDTH * VRAM_HEIGHT), 0);
  std::vector<std::uint32_t> upload_pixels = std::vector<std::uint32_t>(static_cast<std::size_t>(VRAM_WIDTH * VRAM_HEIGHT), 0xFF000000u);
  Gp0State gp0_state{};
  int display_x = 0;
  int display_y = 0;
  int display_w = 320;
  int display_h = 240;
  int display_h_start = 0x260;
  int display_h_end = 0xC60;
  int display_v_start = 0x018;
  int display_v_end = 0x108;

  void throw_sdl_error(const char *message)
  {
    throw std::runtime_error(std::string(message) + ": " + SDL_GetError());
  }

  void init_sdl()
  {
    if (!SDL_Init(SDL_INIT_VIDEO))
    {
      throw_sdl_error("failed to initialize SDL");
    }
  }

  void thread_main()
  {
    try
    {
      thread_loop();
      cleanup();
    }
    catch (const std::exception &e)
    {
      std::fprintf(stderr, "Renderer error: %s\n", e.what());
      cleanup();
      close_requested.store(true);
    }
  }

  void create_window()
  {
    window = SDL_CreateWindow("Camlstation", WIDTH, HEIGHT,
                              SDL_WINDOW_RESIZABLE | SDL_WINDOW_HIGH_PIXEL_DENSITY);
    if (window == nullptr)
    {
      throw_sdl_error("failed to create window");
    }

    window_id = SDL_GetWindowID(window);
    if (window_id == 0)
    {
      throw_sdl_error("failed to query window id");
    }
  }

  void create_renderer()
  {
    renderer = SDL_CreateRenderer(window, "software");
    if (renderer == nullptr)
    {
      renderer = SDL_CreateRenderer(window, nullptr);
    }
    if (renderer == nullptr)
    {
      throw_sdl_error("failed to create SDL renderer");
    }
  }

  void create_vram_texture()
  {
    vram_texture = SDL_CreateTexture(renderer, SDL_PIXELFORMAT_ARGB8888, SDL_TEXTUREACCESS_STREAMING, VRAM_WIDTH, VRAM_HEIGHT);
    if (vram_texture == nullptr)
    {
      throw_sdl_error("failed to create VRAM texture");
    }
  }

  static std::uint16_t rgb24_to_rgb555(std::uint32_t rgb)
  {
    const std::uint32_t r8 = rgb & 0xFFu;
    const std::uint32_t g8 = (rgb >> 8) & 0xFFu;
    const std::uint32_t b8 = (rgb >> 16) & 0xFFu;
    const std::uint16_t r5 = static_cast<std::uint16_t>((r8 * 31u + 127u) / 255u);
    const std::uint16_t g5 = static_cast<std::uint16_t>((g8 * 31u + 127u) / 255u);
    const std::uint16_t b5 = static_cast<std::uint16_t>((b8 * 31u + 127u) / 255u);
    return static_cast<std::uint16_t>(r5 | (g5 << 5) | (b5 << 10));
  }

  static std::uint8_t five_to_eight(std::uint16_t x)
  {
    return static_cast<std::uint8_t>((x * 255u + 15u) / 31u);
  }

  static std::uint32_t rgb555_to_argb32(std::uint16_t p)
  {
    const std::uint8_t r = five_to_eight(static_cast<std::uint16_t>(p & 0x1Fu));
    const std::uint8_t g = five_to_eight(static_cast<std::uint16_t>((p >> 5) & 0x1Fu));
    const std::uint8_t b = five_to_eight(static_cast<std::uint16_t>((p >> 10) & 0x1Fu));
    return 0xFF000000u | (static_cast<std::uint32_t>(r) << 16) |
           (static_cast<std::uint32_t>(g) << 8) |
           static_cast<std::uint32_t>(b);
  }

  void fill_rect(int x, int y, int w, int h, std::uint16_t color)
  {
    if (w <= 0 || h <= 0)
    {
      return;
    }

    const int x0 = std::max(0, x);
    const int y0 = std::max(0, y);
    const int x1 = std::min(VRAM_WIDTH, x + w);
    const int y1 = std::min(VRAM_HEIGHT, y + h);
    for (int py = y0; py < y1; ++py)
    {
      const int row = py * VRAM_WIDTH;
      for (int px = x0; px < x1; ++px)
      {
        vram[static_cast<std::size_t>(row + px)] = color;
      }
    }
  }

  static int gp0_param_words(std::uint8_t opcode)
  {
    if (opcode == 0x02 || opcode == 0xA0)
    {
      return 2;
    }
    if ((opcode & 0xE0u) == 0x80u)
    {
      return 3;
    }
    if ((opcode & 0xF8u) == 0x60u)
    {
      return 2;
    }
    if ((opcode & 0xF8u) == 0x68u || (opcode & 0xF8u) == 0x70u || (opcode & 0xF8u) == 0x78u)
    {
      return 1;
    }
    return 0;
  }

  void write_vram_pixel(int x, int y, std::uint16_t value)
  {
    if (x < 0 || x >= VRAM_WIDTH || y < 0 || y >= VRAM_HEIGHT)
    {
      return;
    }
    vram[static_cast<std::size_t>(y * VRAM_WIDTH + x)] = value;
  }

  void begin_gp0_image_load(std::uint32_t arg0, std::uint32_t arg1)
  {
    gp0_state.image_x = static_cast<int>(arg0 & 0x3FFu);
    gp0_state.image_y = static_cast<int>((arg0 >> 16) & 0x1FFu);
    gp0_state.image_w = static_cast<int>(arg1 & 0xFFFFu);
    gp0_state.image_h = static_cast<int>((arg1 >> 16) & 0xFFFFu);

    if (gp0_state.image_w <= 0 || gp0_state.image_h <= 0)
    {
      gp0_state.image_load_active = false;
      gp0_state.image_words_remaining = 0;
      return;
    }

    const int total_pixels = gp0_state.image_w * gp0_state.image_h;
    gp0_state.image_words_remaining = (total_pixels + 1) / 2;
    gp0_state.image_cur_x = 0;
    gp0_state.image_cur_y = 0;
    gp0_state.image_load_active = gp0_state.image_words_remaining > 0;
  }

  void advance_image_cursor()
  {
    gp0_state.image_cur_x += 1;
    if (gp0_state.image_cur_x >= gp0_state.image_w)
    {
      gp0_state.image_cur_x = 0;
      gp0_state.image_cur_y += 1;
    }
  }

  void consume_gp0_image_word(std::uint32_t word)
  {
    if (!gp0_state.image_load_active)
    {
      return;
    }

    const std::uint16_t px0 = static_cast<std::uint16_t>(word & 0xFFFFu);
    const std::uint16_t px1 = static_cast<std::uint16_t>((word >> 16) & 0xFFFFu);

    write_vram_pixel(gp0_state.image_x + gp0_state.image_cur_x,
                     gp0_state.image_y + gp0_state.image_cur_y, px0);
    advance_image_cursor();

    if (gp0_state.image_cur_y < gp0_state.image_h)
    {
      write_vram_pixel(gp0_state.image_x + gp0_state.image_cur_x,
                       gp0_state.image_y + gp0_state.image_cur_y, px1);
      advance_image_cursor();
    }

    gp0_state.image_words_remaining -= 1;
    if (gp0_state.image_words_remaining <= 0 || gp0_state.image_cur_y >= gp0_state.image_h)
    {
      gp0_state.image_words_remaining = 0;
      gp0_state.image_load_active = false;
    }
  }

  void execute_gp0_fill(std::uint32_t first_word, std::uint32_t arg0, std::uint32_t arg1)
  {
    const std::uint16_t color = rgb24_to_rgb555(first_word & 0x00FFFFFFu);
    int x = static_cast<int>(arg0 & 0x3FFu);
    int y = static_cast<int>((arg0 >> 16) & 0x1FFu);
    int w = static_cast<int>(arg1 & 0x3FFu);
    int h = static_cast<int>((arg1 >> 16) & 0x1FFu);

    if (w == 0)
    {
      w = VRAM_WIDTH;
    }
    if (h == 0)
    {
      h = VRAM_HEIGHT;
    }

    fill_rect(x, y, w, h, color);
  }

  void execute_gp0_rect(std::uint32_t first_word, std::uint8_t opcode, std::uint32_t arg0, std::uint32_t arg1)
  {
    const std::uint16_t color = rgb24_to_rgb555(first_word & 0x00FFFFFFu);
    const int x = static_cast<int>(arg0 & 0x3FFu);
    const int y = static_cast<int>((arg0 >> 16) & 0x1FFu);

    int w = 0;
    int h = 0;
    if ((opcode & 0xF8u) == 0x68u)
    {
      w = 1;
      h = 1;
    }
    else if ((opcode & 0xF8u) == 0x70u)
    {
      w = 8;
      h = 8;
    }
    else if ((opcode & 0xF8u) == 0x78u)
    {
      w = 16;
      h = 16;
    }
    else
    {
      w = static_cast<int>(arg1 & 0xFFFFu);
      h = static_cast<int>((arg1 >> 16) & 0xFFFFu);
    }

    if (w <= 0 || h <= 0)
    {
      return;
    }

    fill_rect(x, y, w, h, color);
  }

  void execute_gp0_vram_copy(std::uint32_t src_xy, std::uint32_t dst_xy, std::uint32_t wh)
  {
    const int src_x = static_cast<int>(src_xy & 0x3FFu);
    const int src_y = static_cast<int>((src_xy >> 16) & 0x1FFu);
    const int dst_x = static_cast<int>(dst_xy & 0x3FFu);
    const int dst_y = static_cast<int>((dst_xy >> 16) & 0x1FFu);
    int w = static_cast<int>(wh & 0xFFFFu);
    int h = static_cast<int>((wh >> 16) & 0xFFFFu);

    if (w <= 0)
    {
      w = VRAM_WIDTH;
    }
    if (h <= 0)
    {
      h = VRAM_HEIGHT;
    }

    // Use a temporary buffer to preserve correct behavior for overlapping regions.
    std::vector<std::uint16_t> temp;
    temp.reserve(static_cast<std::size_t>(w * h));
    for (int y = 0; y < h; ++y)
    {
      for (int x = 0; x < w; ++x)
      {
        const int sx = src_x + x;
        const int sy = src_y + y;
        if (sx >= 0 && sx < VRAM_WIDTH && sy >= 0 && sy < VRAM_HEIGHT)
        {
          temp.push_back(vram[static_cast<std::size_t>(sy * VRAM_WIDTH + sx)]);
        }
        else
        {
          temp.push_back(0);
        }
      }
    }

    std::size_t idx = 0;
    for (int y = 0; y < h; ++y)
    {
      for (int x = 0; x < w; ++x)
      {
        const int tx = dst_x + x;
        const int ty = dst_y + y;
        if (tx >= 0 && tx < VRAM_WIDTH && ty >= 0 && ty < VRAM_HEIGHT)
        {
          vram[static_cast<std::size_t>(ty * VRAM_WIDTH + tx)] = temp[idx];
        }
        idx += 1;
      }
    }
  }

  void process_gp0_word(std::uint32_t word)
  {
    if (gp0_state.image_load_active)
    {
      consume_gp0_image_word(word);
      return;
    }

    if (gp0_state.words_expected == 0)
    {
      gp0_state.first_word = word;
      gp0_state.args_received = 0;
      const auto opcode = static_cast<std::uint8_t>((word >> 24) & 0xFFu);
      gp0_state.words_expected = gp0_param_words(opcode);
      return;
    }

    if (gp0_state.args_received < static_cast<int>(gp0_state.args.size()))
    {
      gp0_state.args[gp0_state.args_received] = word;
      gp0_state.args_received += 1;
    }

    gp0_state.words_expected -= 1;

    if (gp0_state.words_expected == 0)
    {
      const auto opcode = static_cast<std::uint8_t>((gp0_state.first_word >> 24) & 0xFFu);
      if (opcode == 0x02)
      {
        execute_gp0_fill(gp0_state.first_word, gp0_state.args[0], gp0_state.args[1]);
      }
      else if (opcode == 0xA0)
      {
        begin_gp0_image_load(gp0_state.args[0], gp0_state.args[1]);
      }
      else if ((opcode & 0xE0u) == 0x80u)
      {
        execute_gp0_vram_copy(gp0_state.args[0], gp0_state.args[1], gp0_state.args[2]);
      }
      else if ((opcode & 0xF8u) == 0x60u || (opcode & 0xF8u) == 0x68u ||
               (opcode & 0xF8u) == 0x70u || (opcode & 0xF8u) == 0x78u)
      {
        const std::uint32_t arg0 = gp0_state.args[0];
        const std::uint32_t arg1 = gp0_state.args_received >= 2 ? gp0_state.args[1] : 0;
        execute_gp0_rect(gp0_state.first_word, opcode, arg0, arg1);
      }
    }
  }

  void process_gp1_word(std::uint32_t word)
  {
    const std::uint8_t opcode = static_cast<std::uint8_t>((word >> 24) & 0xFFu);
    switch (opcode)
    {
    case 0x00:
      display_x = 0;
      display_y = 0;
      display_w = 320;
      display_h = 240;
      break;
    case 0x05:
      display_x = static_cast<int>(word & 0x3FFu);
      display_y = static_cast<int>((word >> 10) & 0x1FFu);
      break;
    case 0x06:
    {
      display_h_start = static_cast<int>(word & 0xFFFu);
      display_h_end = static_cast<int>((word >> 12) & 0xFFFu);
      int w = (display_h_end - display_h_start) / 8;
      if (w > 0)
      {
        display_w = w;
      }
      break;
    }
    case 0x07:
    {
      display_v_start = static_cast<int>(word & 0x3FFu);
      display_v_end = static_cast<int>((word >> 10) & 0x3FFu);
      int h = display_v_end - display_v_start;
      if (h > 0)
      {
        display_h = h;
      }
      break;
    }
    case 0x08:
    {
      const int hres_lo = static_cast<int>(word & 0x3u);
      const bool hres_hi = ((word >> 6) & 0x1u) != 0;
      if (hres_hi)
      {
        display_w = 368;
      }
      else
      {
        display_w = (hres_lo == 0 ? 256 : hres_lo == 1 ? 320
                                      : hres_lo == 2   ? 512
                                                       : 640);
      }
      display_h = ((word >> 2) & 0x1u) ? 480 : 240;
      break;
    }
    default:
      break;
    }
  }

  void drain_commands()
  {
    std::queue<int> pending;
    std::queue<int> gp1_pending;
    {
      std::lock_guard<std::mutex> lock(queue_mutex);
      pending.swap(command_queue);
      gp1_pending.swap(gp1_command_queue);
    }

    while (!pending.empty())
    {
      const auto word = static_cast<std::uint32_t>(pending.front());
      process_gp0_word(word);
      pending.pop();
    }

    while (!gp1_pending.empty())
    {
      const auto word = static_cast<std::uint32_t>(gp1_pending.front());
      process_gp1_word(word);
      gp1_pending.pop();
    }
  }

  void thread_loop()
  {
    while (!stop_requested.load() && !close_requested.load())
    {
      SDL_Event event;
      while (SDL_PollEvent(&event))
      {
        if (event.type == SDL_EVENT_WINDOW_CLOSE_REQUESTED && event.window.windowID == window_id)
        {
          close_requested.store(true);
        }
      }

      if (close_requested.load())
      {
        break;
      }

      drain_commands();
      draw_frame();
      SDL_Delay(16);
    }
  }

  void draw_frame()
  {
    const std::size_t total_pixels = static_cast<std::size_t>(VRAM_WIDTH * VRAM_HEIGHT);
    for (std::size_t i = 0; i < total_pixels; ++i)
    {
      upload_pixels[i] = rgb555_to_argb32(vram[i]);
    }

    if (!SDL_UpdateTexture(vram_texture, nullptr, upload_pixels.data(), VRAM_WIDTH * static_cast<int>(sizeof(std::uint32_t))))
    {
      throw_sdl_error("failed to update VRAM texture");
    }

    int window_width = 0;
    int window_height = 0;
    if (!SDL_GetWindowSizeInPixels(window, &window_width, &window_height))
    {
      throw_sdl_error("failed to query window size");
    }

    if (!SDL_SetRenderDrawColor(renderer, 18, 18, 24, 255))
    {
      throw_sdl_error("failed to set clear color");
    }
    if (!SDL_RenderClear(renderer))
    {
      throw_sdl_error("failed to clear renderer");
    }

    const int src_x = std::clamp(display_x, 0, VRAM_WIDTH - 1);
    const int src_y = std::clamp(display_y, 0, VRAM_HEIGHT - 1);
    const int src_w = std::clamp(display_w, 1, VRAM_WIDTH - src_x);
    const int src_h = std::clamp(display_h, 1, VRAM_HEIGHT - src_y);

    SDL_FRect src_rect{};
    src_rect.x = static_cast<float>(src_x);
    src_rect.y = static_cast<float>(src_y);
    src_rect.w = static_cast<float>(src_w);
    src_rect.h = static_cast<float>(src_h);

    SDL_FRect dst_rect{};
    dst_rect.x = 0.0f;
    dst_rect.y = 0.0f;
    dst_rect.w = static_cast<float>(window_width);
    dst_rect.h = static_cast<float>(window_height);

    if (!SDL_RenderTexture(renderer, vram_texture, &src_rect, &dst_rect))
    {
      throw_sdl_error("failed to render VRAM texture");
    }
    if (!SDL_RenderPresent(renderer))
    {
      throw_sdl_error("failed to present frame");
    }
  }

  void cleanup()
  {
    if (vram_texture != nullptr)
    {
      SDL_DestroyTexture(vram_texture);
      vram_texture = nullptr;
    }

    if (renderer != nullptr)
    {
      SDL_DestroyRenderer(renderer);
      renderer = nullptr;
    }

    if (window != nullptr)
    {
      SDL_DestroyWindow(window);
      window = nullptr;
    }

    SDL_Quit();
  }
};

// OCaml-facing interface

static std::mutex renderer_mutex;
static std::unique_ptr<Renderer> renderer_instance;

extern "C" CAMLprim value init_renderer(value unit)
{
  CAMLparam1(unit);
  {
    std::lock_guard<std::mutex> lock(renderer_mutex);
    if (!renderer_instance)
    {
      renderer_instance = std::make_unique<Renderer>();
    }
  }
  CAMLreturn(Val_unit);
}

extern "C" CAMLprim value submit_command(value command)
{
  CAMLparam1(command);
  int cmd = Int_val(command);
  {
    std::lock_guard<std::mutex> lock(renderer_mutex);
    if (renderer_instance.get() == nullptr)
    {
      caml_invalid_argument("submit_command: renderer not initialized");
    }
    renderer_instance->submit_command(cmd);
  }
  CAMLreturn(Val_unit);
}

extern "C" CAMLprim value submit_gp1_command(value command)
{
  CAMLparam1(command);
  int cmd = Int_val(command);
  {
    std::lock_guard<std::mutex> lock(renderer_mutex);
    if (renderer_instance.get() == nullptr)
    {
      caml_invalid_argument("submit_gp1_command: renderer not initialized");
    }
    renderer_instance->submit_gp1_command(cmd);
  }
  CAMLreturn(Val_unit);
}

extern "C" CAMLprim value should_close(value unit)
{
  CAMLparam1(unit);
  if (renderer_instance.get() == nullptr)
  {
    caml_invalid_argument("should_close: renderer not initialized");
  }
  bool should_close = renderer_instance->should_close();
  CAMLreturn(Val_bool(should_close));
}
