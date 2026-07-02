#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <algorithm>
#include <array>
#include <atomic>
#include <cmath>
#include <cstdint>
#include <cstdio>
#include <memory>
#include <mutex>
#include <queue>
#include <stdexcept>
#include <string>
#include <string_view>
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
  }

  ~Renderer()
  {
    shutdown();
  }

  void shutdown()
  {
    cleanup();
  }

  bool should_close() const
  {
    return close_requested.load();
  }

  void poll_events()
  {
    SDL_Event event;
    while (SDL_PollEvent(&event))
    {
      if (event.type == SDL_EVENT_WINDOW_CLOSE_REQUESTED &&
          event.window.windowID == window_id)
      {
        close_requested.store(true);
      }
    }
  }

  void present_frame(value pixels, int src_x, int src_y, int src_w, int src_h)
  {
    const std::size_t total_pixels = static_cast<std::size_t>(VRAM_WIDTH * VRAM_HEIGHT);
    if (static_cast<std::size_t>(Wosize_val(pixels)) != total_pixels)
    {
      caml_invalid_argument("renderer_present_frame: unexpected pixel buffer size");
    }

    for (std::size_t i = 0; i < total_pixels; ++i)
    {
      upload_pixels[i] = static_cast<std::uint32_t>(Long_val(Field(pixels, static_cast<long>(i))));
    }

    if (!SDL_UpdateTexture(vram_texture, nullptr, upload_pixels.data(),
                           VRAM_WIDTH * static_cast<int>(sizeof(std::uint32_t))))
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

    const int clamped_src_x = std::clamp(src_x, 0, VRAM_WIDTH - 1);
    const int clamped_src_y = std::clamp(src_y, 0, VRAM_HEIGHT - 1);
    const int clamped_src_w = std::clamp(src_w, 1, VRAM_WIDTH - clamped_src_x);
    const int clamped_src_h = std::clamp(src_h, 1, VRAM_HEIGHT - clamped_src_y);

    SDL_FRect src_rect{};
    src_rect.x = static_cast<float>(clamped_src_x);
    src_rect.y = static_cast<float>(clamped_src_y);
    src_rect.w = static_cast<float>(clamped_src_w);
    src_rect.h = static_cast<float>(clamped_src_h);

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

private:
  static constexpr int VRAM_WIDTH = 1024;
  static constexpr int VRAM_HEIGHT = 512;
  const int WIDTH = 800;
  const int HEIGHT = 600;

  enum class RenderCommandType
  {
    Fill,
    Rect,
    LineFlat,
    LineShaded,
    PolygonFlatTri,
    PolygonShadedTri,
    PolygonFlatQuad,
    PolygonShadedQuad,
    DrawAreaTopLeft,
    DrawAreaBottomRight,
    DrawMode,
    VramCopy,
    ImageBegin,
    ImageWord,
    DisplayReset,
    DisplayArea,
    DisplayHRange,
    DisplayVRange,
    DisplayMode
  };

  struct RenderCommand
  {
    RenderCommandType type;
    std::array<int, 10> args{};
  };

  struct QuadVertex
  {
    int x;
    int y;
    std::uint16_t color;
  };

  std::atomic_bool close_requested{false};

  SDL_Window *window = nullptr;
  SDL_Renderer *renderer = nullptr;
  SDL_Texture *vram_texture = nullptr;
  Uint32 window_id = 0;
  std::vector<std::uint32_t> upload_pixels = std::vector<std::uint32_t>(static_cast<std::size_t>(VRAM_WIDTH * VRAM_HEIGHT), 0xFF000000u);

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

  static std::uint32_t rgb555_to_argb32(std::uint16_t p)
  {
    const std::uint8_t r = five_to_eight(static_cast<std::uint16_t>(p & 0x1Fu));
    const std::uint8_t g = five_to_eight(static_cast<std::uint16_t>((p >> 5) & 0x1Fu));
    const std::uint8_t b = five_to_eight(static_cast<std::uint16_t>((p >> 10) & 0x1Fu));
    return 0xFF000000u | (static_cast<std::uint32_t>(r) << 16) |
           (static_cast<std::uint32_t>(g) << 8) |
           static_cast<std::uint32_t>(b);
  }

  static std::uint8_t five_to_eight(std::uint16_t x)
  {
    return static_cast<std::uint8_t>((x * 255u + 15u) / 31u);
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

extern "C" CAMLprim value renderer_poll_events(value unit)
{
  CAMLparam1(unit);
  if (renderer_instance.get() == nullptr)
  {
    caml_invalid_argument("renderer_poll_events: renderer not initialized");
  }
  renderer_instance->poll_events();
  CAMLreturn(Val_unit);
}

extern "C" CAMLprim value renderer_present_frame(value pixels, value src_x,
                                                 value src_y, value src_w,
                                                 value src_h)
{
  CAMLparam5(pixels, src_x, src_y, src_w, src_h);
  if (renderer_instance.get() == nullptr)
  {
    caml_invalid_argument("renderer_present_frame: renderer not initialized");
  }
  renderer_instance->present_frame(pixels, Int_val(src_x), Int_val(src_y),
                                   Int_val(src_w), Int_val(src_h));
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
