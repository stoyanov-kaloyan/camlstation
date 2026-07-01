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
    worker_thread = std::thread(&Renderer::thread_main, this);
  }

  ~Renderer()
  {
    shutdown();
  }

  void submit_named(std::string_view name, int a0, int a1, int a2, int a3)
  {
    submit_render_command(decode_named_command(name), {a0, a1, a2, a3});
  }

  void submit_polygon_flat(int semi, int color, int xy0, int xy1, int xy2)
  {
    submit_render_command(RenderCommandType::PolygonFlatTri,
                          {semi, color, xy0, xy1, xy2, 0, 0, 0});
  }

  void submit_polygon_shaded(int semi, int color0, int xy0, int color1,
                             int xy1, int color2, int xy2)
  {
    submit_render_command(RenderCommandType::PolygonShadedTri,
                          {semi, color0, xy0, color1, xy1, color2, xy2});
  }

  void submit_polygon_flat_quad(int semi, int color, int xy0, int xy1,
                                int xy2, int xy3)
  {
    submit_render_command(RenderCommandType::PolygonFlatQuad,
                          {semi, color, xy0, xy1, xy2, xy3});
  }

  void submit_polygon_shaded_quad(int semi, int color0, int xy0, int color1,
                                  int xy1, int color2, int xy2, int color3,
                                  int xy3)
  {
    submit_render_command(RenderCommandType::PolygonShadedQuad,
                          {semi, color0, xy0, color1, xy1, color2, xy2,
                           color3, xy3});
  }

  void submit_draw_area_top_left(int packed_xy)
  {
    submit_render_command(RenderCommandType::DrawAreaTopLeft,
                          {packed_xy, 0, 0, 0, 0, 0, 0, 0, 0, 0});
  }

  void submit_draw_area_bottom_right(int packed_xy)
  {
    submit_render_command(RenderCommandType::DrawAreaBottomRight,
                          {packed_xy, 0, 0, 0, 0, 0, 0, 0, 0, 0});
  }

  void submit_draw_mode(int draw_mode)
  {
    submit_render_command(RenderCommandType::DrawMode,
                          {draw_mode, 0, 0, 0, 0, 0, 0, 0, 0, 0});
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

  struct ImageTransferState
  {
    bool image_load_active = false;
    int image_x = 0;
    int image_y = 0;
    int image_w = 0;
    int image_h = 0;
    int image_cur_x = 0;
    int image_cur_y = 0;
    int image_words_remaining = 0;
  };

  std::queue<RenderCommand> render_command_queue;
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
  ImageTransferState image_state{};
  int draw_area_left = 0;
  int draw_area_top = 0;
  int draw_area_right = VRAM_WIDTH - 1;
  int draw_area_bottom = VRAM_HEIGHT - 1;
  bool dither_enabled = false;
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

  static std::uint16_t blend_rgb555(std::uint16_t src, std::uint16_t dst)
  {
    const std::uint16_t sr = static_cast<std::uint16_t>(src & 0x1Fu);
    const std::uint16_t sg = static_cast<std::uint16_t>((src >> 5) & 0x1Fu);
    const std::uint16_t sb = static_cast<std::uint16_t>((src >> 10) & 0x1Fu);
    const std::uint16_t dr = static_cast<std::uint16_t>(dst & 0x1Fu);
    const std::uint16_t dg = static_cast<std::uint16_t>((dst >> 5) & 0x1Fu);
    const std::uint16_t db = static_cast<std::uint16_t>((dst >> 10) & 0x1Fu);
    return static_cast<std::uint16_t>((sr + dr) / 2 | (((sg + dg) / 2) << 5) |
                                      (((sb + db) / 2) << 10));
  }

  static int dither_offset(int x, int y)
  {
    static constexpr int pattern[4][4] = {
        {-4, 0, -3, 1},
        {2, -2, 3, -1},
        {-3, 1, -4, 0},
        {3, -1, 2, -2},
    };
    return pattern[y & 3][x & 3];
  }

  bool in_draw_area(int x, int y) const
  {
    return x >= draw_area_left && x <= draw_area_right &&
           y >= draw_area_top && y <= draw_area_bottom;
  }

  void submit_render_command(RenderCommandType type, std::array<int, 10> args)
  {
    std::lock_guard<std::mutex> lock(queue_mutex);
    render_command_queue.push(RenderCommand{type, args});
  }

  // FNV-1a hash function for string hashing
  // used to efficiently map command names to enum values
  static constexpr std::uint64_t fnv1a_const(const char *s, std::size_t n)
  {
    std::uint64_t h = 14695981039346656037ull;
    for (std::size_t i = 0; i < n; ++i)
    {
      h ^= static_cast<std::uint8_t>(s[i]);
      h *= 1099511628211ull;
    }
    return h;
  }

  // Runtime version of FNV-1a for string_view hashing
  static std::uint64_t fnv1a_runtime(std::string_view s)
  {
    std::uint64_t h = 14695981039346656037ull;
    for (const char c : s)
    {
      h ^= static_cast<std::uint8_t>(c);
      h *= 1099511628211ull;
    }
    return h;
  }

  static RenderCommandType decode_named_command(std::string_view name)
  {
    const std::uint64_t key = fnv1a_runtime(name);
    // we hash the strings so we can switch on them and dispatch function calls
    switch (key)
    {
    case fnv1a_const("fill", 4):
      if (name == "fill")
        return RenderCommandType::Fill;
      break;
    case fnv1a_const("rect", 4):
      if (name == "rect")
        return RenderCommandType::Rect;
      break;
    case fnv1a_const("line_flat", 9):
      if (name == "line_flat")
        return RenderCommandType::LineFlat;
      break;
    case fnv1a_const("line_shaded", 11):
      if (name == "line_shaded")
        return RenderCommandType::LineShaded;
      break;
    case fnv1a_const("vram_copy", 9):
      if (name == "vram_copy")
        return RenderCommandType::VramCopy;
      break;
    case fnv1a_const("image_begin", 11):
      if (name == "image_begin")
        return RenderCommandType::ImageBegin;
      break;
    case fnv1a_const("image_word", 10):
      if (name == "image_word")
        return RenderCommandType::ImageWord;
      break;
    case fnv1a_const("display_reset", 13):
      if (name == "display_reset")
        return RenderCommandType::DisplayReset;
      break;
    case fnv1a_const("display_area", 12):
      if (name == "display_area")
        return RenderCommandType::DisplayArea;
      break;
    case fnv1a_const("display_h_range", 15):
      if (name == "display_h_range")
        return RenderCommandType::DisplayHRange;
      break;
    case fnv1a_const("display_v_range", 15):
      if (name == "display_v_range")
        return RenderCommandType::DisplayVRange;
      break;
    case fnv1a_const("display_mode", 12):
      if (name == "display_mode")
        return RenderCommandType::DisplayMode;
      break;
    default:
      break;
    }

    throw std::invalid_argument(std::string("unknown renderer command: ") +
                                std::string(name));
  }

  void fill_rect(int x, int y, int w, int h, std::uint16_t color)
  {
    if (w <= 0 || h <= 0)
    {
      return;
    }

    const int x0 = std::max({0, x, draw_area_left});
    const int y0 = std::max({0, y, draw_area_top});
    const int x1 = std::min({VRAM_WIDTH - 1, x + w - 1, draw_area_right});
    const int y1 = std::min({VRAM_HEIGHT - 1, y + h - 1, draw_area_bottom});
    for (int py = y0; py <= y1; ++py)
    {
      const int row = py * VRAM_WIDTH;
      for (int px = x0; px <= x1; ++px)
      {
        vram[static_cast<std::size_t>(row + px)] = color;
      }
    }
  }

  static int gp0_decode_x(std::uint32_t xy)
  {
    return static_cast<int>(xy & 0x3FFu);
  }

  static int gp0_decode_y(std::uint32_t xy)
  {
    return static_cast<int>((xy >> 16) & 0x1FFu);
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
    image_state.image_x = static_cast<int>(arg0 & 0x3FFu);
    image_state.image_y = static_cast<int>((arg0 >> 16) & 0x1FFu);
    image_state.image_w = static_cast<int>(arg1 & 0xFFFFu);
    image_state.image_h = static_cast<int>((arg1 >> 16) & 0xFFFFu);

    if (image_state.image_w <= 0 || image_state.image_h <= 0)
    {
      image_state.image_load_active = false;
      image_state.image_words_remaining = 0;
      return;
    }

    const int total_pixels = image_state.image_w * image_state.image_h;
    image_state.image_words_remaining = (total_pixels + 1) / 2;
    image_state.image_cur_x = 0;
    image_state.image_cur_y = 0;
    image_state.image_load_active = image_state.image_words_remaining > 0;
  }

  void advance_image_cursor()
  {
    image_state.image_cur_x += 1;
    if (image_state.image_cur_x >= image_state.image_w)
    {
      image_state.image_cur_x = 0;
      image_state.image_cur_y += 1;
    }
  }

  void consume_gp0_image_word(std::uint32_t word)
  {
    if (!image_state.image_load_active)
    {
      return;
    }

    const std::uint16_t px0 = static_cast<std::uint16_t>(word & 0xFFFFu);
    const std::uint16_t px1 = static_cast<std::uint16_t>((word >> 16) & 0xFFFFu);

    write_vram_pixel(image_state.image_x + image_state.image_cur_x,
                     image_state.image_y + image_state.image_cur_y, px0);
    advance_image_cursor();

    if (image_state.image_cur_y < image_state.image_h)
    {
      write_vram_pixel(image_state.image_x + image_state.image_cur_x,
                       image_state.image_y + image_state.image_cur_y, px1);
      advance_image_cursor();
    }

    image_state.image_words_remaining -= 1;
    if (image_state.image_words_remaining <= 0 || image_state.image_cur_y >= image_state.image_h)
    {
      image_state.image_words_remaining = 0;
      image_state.image_load_active = false;
    }
  }

  void draw_line_flat(int x0, int y0, int x1, int y1, std::uint16_t color)
  {
    // Bresenham with a constant color.
    int dx = std::abs(x1 - x0);
    int dy = -std::abs(y1 - y0);
    int sx = (x0 < x1) ? 1 : -1;
    int sy = (y0 < y1) ? 1 : -1;
    int err = dx + dy;

    int x = x0;
    int y = y0;

    while (true)
    {
      write_vram_pixel(x, y, color);
      if (x == x1 && y == y1)
      {
        break;
      }
      int e2 = 2 * err;
      if (e2 >= dy)
      {
        err += dy;
        x += sx;
      }
      if (e2 <= dx)
      {
        err += dx;
        y += sy;
      }
    }
  }

  void draw_line_shaded(int x0, int y0, std::uint16_t c0, int x1, int y1, std::uint16_t c1)
  {
    const int steps = std::max(std::abs(x1 - x0), std::abs(y1 - y0));
    if (steps == 0)
    {
      write_vram_pixel(x0, y0, c0);
      return;
    }

    const int r0 = c0 & 0x1F;
    const int g0 = (c0 >> 5) & 0x1F;
    const int b0 = (c0 >> 10) & 0x1F;
    const int r1 = c1 & 0x1F;
    const int g1 = (c1 >> 5) & 0x1F;
    const int b1 = (c1 >> 10) & 0x1F;

    for (int i = 0; i <= steps; ++i)
    {
      const float t = static_cast<float>(i) / static_cast<float>(steps);
      const int x = static_cast<int>(std::lround(x0 + (x1 - x0) * t));
      const int y = static_cast<int>(std::lround(y0 + (y1 - y0) * t));
      const int r = static_cast<int>(std::lround(r0 + (r1 - r0) * t));
      const int g = static_cast<int>(std::lround(g0 + (g1 - g0) * t));
      const int b = static_cast<int>(std::lround(b0 + (b1 - b0) * t));
      const std::uint16_t c = static_cast<std::uint16_t>((r & 0x1F) | ((g & 0x1F) << 5) | ((b & 0x1F) << 10));
      write_vram_pixel(x, y, c);
    }
  }

  struct TriangleVertex
  {
    int x;
    int y;
    std::uint16_t color;
  };

  static bool is_top_left_edge(const TriangleVertex &a, const TriangleVertex &b)
  {
    return (a.y < b.y) || (a.y == b.y && a.x > b.x);
  }

  void draw_filled_triangle(const TriangleVertex &v0, const TriangleVertex &v1,
                            const TriangleVertex &v2, bool semi_transparent)
  {
    TriangleVertex a = v0;
    TriangleVertex b = v1;
    TriangleVertex c = v2;

    int area = (b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x);
    if (area == 0)
    {
      return;
    }
    if (area < 0)
    {
      std::swap(b, c);
      area = -area;
    }

    const int min_x = std::clamp(std::max({0, draw_area_left, std::min({a.x, b.x, c.x})}),
                                 0, VRAM_WIDTH - 1);
    const int max_x = std::clamp(std::min({VRAM_WIDTH - 1, draw_area_right,
                                           std::max({a.x, b.x, c.x})}),
                                 0, VRAM_WIDTH - 1);
    const int min_y = std::clamp(std::max({0, draw_area_top, std::min({a.y, b.y, c.y})}),
                                 0, VRAM_HEIGHT - 1);
    const int max_y = std::clamp(std::min({VRAM_HEIGHT - 1, draw_area_bottom,
                                           std::max({a.y, b.y, c.y})}),
                                 0, VRAM_HEIGHT - 1);
    const double inv_area = 1.0 / static_cast<double>(area);
    const int r0 = static_cast<int>(a.color & 0x1Fu);
    const int g0 = static_cast<int>((a.color >> 5) & 0x1Fu);
    const int b0 = static_cast<int>((a.color >> 10) & 0x1Fu);
    const int r1 = static_cast<int>(b.color & 0x1Fu);
    const int g1 = static_cast<int>((b.color >> 5) & 0x1Fu);
    const int b1 = static_cast<int>((b.color >> 10) & 0x1Fu);
    const int r2 = static_cast<int>(c.color & 0x1Fu);
    const int g2 = static_cast<int>((c.color >> 5) & 0x1Fu);
    const int b2 = static_cast<int>((c.color >> 10) & 0x1Fu);
    auto edge = [](const TriangleVertex &p0, const TriangleVertex &p1, int px,
                   int py) -> int
    {
      return (p1.x - p0.x) * (py - p0.y) - (p1.y - p0.y) * (px - p0.x);
    };

    for (int y = min_y; y <= max_y; ++y)
    {
      for (int x = min_x; x <= max_x; ++x)
      {
        const int w0 = edge(b, c, x, y);
        const int w1 = edge(c, a, x, y);
        const int w2 = edge(a, b, x, y);
        const bool inside = (w0 > 0 || (w0 == 0 && is_top_left_edge(b, c))) &&
                            (w1 > 0 || (w1 == 0 && is_top_left_edge(c, a))) &&
                            (w2 > 0 || (w2 == 0 && is_top_left_edge(a, b)));
        if (!inside)
        {
          continue;
        }

        const double wa = static_cast<double>(w0) * inv_area;
        const double wb = static_cast<double>(w1) * inv_area;
        const double wc = static_cast<double>(w2) * inv_area;
        int r = static_cast<int>(std::lround(r0 * wa + r1 * wb + r2 * wc));
        int g = static_cast<int>(std::lround(g0 * wa + g1 * wb + g2 * wc));
        int bch = static_cast<int>(std::lround(b0 * wa + b1 * wb + b2 * wc));
        if (dither_enabled)
        {
          const int delta = dither_offset(x, y);
          r = std::clamp(r + delta, 0, 31);
          g = std::clamp(g + delta, 0, 31);
          bch = std::clamp(bch + delta, 0, 31);
        }
        else
        {
          r = std::clamp(r, 0, 31);
          g = std::clamp(g, 0, 31);
          bch = std::clamp(bch, 0, 31);
        }
        std::uint16_t color = static_cast<std::uint16_t>(r | (g << 5) | (bch << 10));
        if (semi_transparent)
        {
          const std::uint16_t dst = vram[static_cast<std::size_t>(y * VRAM_WIDTH + x)];
          color = blend_rgb555(color, dst);
        }
        vram[static_cast<std::size_t>(y * VRAM_WIDTH + x)] = color;
      }
    }
  }

  void draw_filled_quad(const QuadVertex &v0, const QuadVertex &v1,
                        const QuadVertex &v2, const QuadVertex &v3,
                        bool semi_transparent)
  {
    draw_filled_triangle(TriangleVertex{v0.x, v0.y, v0.color},
                         TriangleVertex{v1.x, v1.y, v1.color},
                         TriangleVertex{v2.x, v2.y, v2.color},
                         semi_transparent);
    draw_filled_triangle(TriangleVertex{v1.x, v1.y, v1.color},
                         TriangleVertex{v2.x, v2.y, v2.color},
                         TriangleVertex{v3.x, v3.y, v3.color},
                         semi_transparent);
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

  void drain_commands()
  {
    std::queue<RenderCommand> render_pending;
    {
      std::lock_guard<std::mutex> lock(queue_mutex);
      render_pending.swap(render_command_queue);
    }

    while (!render_pending.empty())
    {
      const RenderCommand cmd = render_pending.front();
      switch (cmd.type)
      {
      case RenderCommandType::Fill:
        execute_gp0_fill(static_cast<std::uint32_t>(cmd.args[0]) & 0x00FFFFFFu,
                         static_cast<std::uint32_t>(cmd.args[1]),
                         static_cast<std::uint32_t>(cmd.args[2]));
        break;
      case RenderCommandType::Rect:
      {
        const bool semi_transparent = cmd.args[0] != 0;
        const std::uint16_t color =
            rgb24_to_rgb555(static_cast<std::uint32_t>(cmd.args[1]) & 0x00FFFFFFu);
        const std::uint32_t xy = static_cast<std::uint32_t>(cmd.args[2]);
        const std::uint32_t wh = static_cast<std::uint32_t>(cmd.args[3]);
        const int x = static_cast<int>(xy & 0x3FFu);
        const int y = static_cast<int>((xy >> 16) & 0x1FFu);
        const int w = static_cast<int>(wh & 0xFFFFu);
        const int h = static_cast<int>((wh >> 16) & 0xFFFFu);
        if (w > 0 && h > 0)
        {
          if (semi_transparent)
          {
            const int x0 = std::max({0, x, draw_area_left});
            const int y0 = std::max({0, y, draw_area_top});
            const int x1 = std::min({VRAM_WIDTH - 1, x + w - 1, draw_area_right});
            const int y1 = std::min({VRAM_HEIGHT - 1, y + h - 1, draw_area_bottom});
            for (int py = y0; py <= y1; ++py)
            {
              const int row = py * VRAM_WIDTH;
              for (int px = x0; px <= x1; ++px)
              {
                const std::size_t index = static_cast<std::size_t>(row + px);
                vram[index] = blend_rgb555(color, vram[index]);
              }
            }
          }
          else
          {
            fill_rect(x, y, w, h, color);
          }
        }
        break;
      }
      case RenderCommandType::LineFlat:
        draw_line_flat(gp0_decode_x(static_cast<std::uint32_t>(cmd.args[1])),
                       gp0_decode_y(static_cast<std::uint32_t>(cmd.args[1])),
                       gp0_decode_x(static_cast<std::uint32_t>(cmd.args[2])),
                       gp0_decode_y(static_cast<std::uint32_t>(cmd.args[2])),
                       static_cast<std::uint16_t>(cmd.args[0] & 0x7FFF));
        break;
      case RenderCommandType::LineShaded:
        draw_line_shaded(gp0_decode_x(static_cast<std::uint32_t>(cmd.args[2])),
                         gp0_decode_y(static_cast<std::uint32_t>(cmd.args[2])),
                         static_cast<std::uint16_t>(cmd.args[0] & 0x7FFF),
                         gp0_decode_x(static_cast<std::uint32_t>(cmd.args[3])),
                         gp0_decode_y(static_cast<std::uint32_t>(cmd.args[3])),
                         static_cast<std::uint16_t>(cmd.args[1] & 0x7FFF));
        break;
      case RenderCommandType::PolygonFlatTri:
      {
        const bool semi = cmd.args[0] != 0;
        const std::uint16_t color = static_cast<std::uint16_t>(cmd.args[1] & 0x7FFF);
        const TriangleVertex v0{gp0_decode_x(static_cast<std::uint32_t>(cmd.args[2])),
                                gp0_decode_y(static_cast<std::uint32_t>(cmd.args[2])),
                                color};
        const TriangleVertex v1{gp0_decode_x(static_cast<std::uint32_t>(cmd.args[3])),
                                gp0_decode_y(static_cast<std::uint32_t>(cmd.args[3])),
                                color};
        const TriangleVertex v2{gp0_decode_x(static_cast<std::uint32_t>(cmd.args[4])),
                                gp0_decode_y(static_cast<std::uint32_t>(cmd.args[4])),
                                color};
        draw_filled_triangle(v0, v1, v2, semi);
        break;
      }
      case RenderCommandType::PolygonShadedTri:
      {
        const bool semi = cmd.args[0] != 0;
        const TriangleVertex v0{gp0_decode_x(static_cast<std::uint32_t>(cmd.args[2])),
                                gp0_decode_y(static_cast<std::uint32_t>(cmd.args[2])),
                                static_cast<std::uint16_t>(cmd.args[1] & 0x7FFF)};
        const TriangleVertex v1{gp0_decode_x(static_cast<std::uint32_t>(cmd.args[4])),
                                gp0_decode_y(static_cast<std::uint32_t>(cmd.args[4])),
                                static_cast<std::uint16_t>(cmd.args[3] & 0x7FFF)};
        const TriangleVertex v2{gp0_decode_x(static_cast<std::uint32_t>(cmd.args[6])),
                                gp0_decode_y(static_cast<std::uint32_t>(cmd.args[6])),
                                static_cast<std::uint16_t>(cmd.args[5] & 0x7FFF)};
        draw_filled_triangle(v0, v1, v2, semi);
        break;
      }
      case RenderCommandType::PolygonFlatQuad:
      {
        const bool semi = cmd.args[0] != 0;
        const std::uint16_t color = static_cast<std::uint16_t>(cmd.args[1] & 0x7FFF);
        const QuadVertex v0{gp0_decode_x(static_cast<std::uint32_t>(cmd.args[2])),
                            gp0_decode_y(static_cast<std::uint32_t>(cmd.args[2])),
                            color};
        const QuadVertex v1{gp0_decode_x(static_cast<std::uint32_t>(cmd.args[3])),
                            gp0_decode_y(static_cast<std::uint32_t>(cmd.args[3])),
                            color};
        const QuadVertex v2{gp0_decode_x(static_cast<std::uint32_t>(cmd.args[4])),
                            gp0_decode_y(static_cast<std::uint32_t>(cmd.args[4])),
                            color};
        const QuadVertex v3{gp0_decode_x(static_cast<std::uint32_t>(cmd.args[5])),
                            gp0_decode_y(static_cast<std::uint32_t>(cmd.args[5])),
                            color};
        draw_filled_quad(v0, v1, v2, v3, semi);
        break;
      }
      case RenderCommandType::PolygonShadedQuad:
      {
        const bool semi = cmd.args[0] != 0;
        const QuadVertex v0{gp0_decode_x(static_cast<std::uint32_t>(cmd.args[2])),
                            gp0_decode_y(static_cast<std::uint32_t>(cmd.args[2])),
                            static_cast<std::uint16_t>(cmd.args[1] & 0x7FFF)};
        const QuadVertex v1{gp0_decode_x(static_cast<std::uint32_t>(cmd.args[4])),
                            gp0_decode_y(static_cast<std::uint32_t>(cmd.args[4])),
                            static_cast<std::uint16_t>(cmd.args[3] & 0x7FFF)};
        const QuadVertex v2{gp0_decode_x(static_cast<std::uint32_t>(cmd.args[6])),
                            gp0_decode_y(static_cast<std::uint32_t>(cmd.args[6])),
                            static_cast<std::uint16_t>(cmd.args[5] & 0x7FFF)};
        const QuadVertex v3{gp0_decode_x(static_cast<std::uint32_t>(cmd.args[8])),
                            gp0_decode_y(static_cast<std::uint32_t>(cmd.args[8])),
                            static_cast<std::uint16_t>(cmd.args[7] & 0x7FFF)};
        draw_filled_quad(v0, v1, v2, v3, semi);
        break;
      }
      case RenderCommandType::VramCopy:
        execute_gp0_vram_copy(static_cast<std::uint32_t>(cmd.args[0]),
                              static_cast<std::uint32_t>(cmd.args[1]),
                              static_cast<std::uint32_t>(cmd.args[2]));
        break;
      case RenderCommandType::ImageBegin:
        begin_gp0_image_load(static_cast<std::uint32_t>(cmd.args[0]),
                             static_cast<std::uint32_t>(cmd.args[1]));
        break;
      case RenderCommandType::ImageWord:
        consume_gp0_image_word(static_cast<std::uint32_t>(cmd.args[0]));
        break;
      case RenderCommandType::DisplayReset:
        display_x = 0;
        display_y = 0;
        display_w = 320;
        display_h = 240;
        display_h_start = 0x260;
        display_h_end = 0xC60;
        display_v_start = 0x018;
        display_v_end = 0x108;
        draw_area_left = 0;
        draw_area_top = 0;
        draw_area_right = VRAM_WIDTH - 1;
        draw_area_bottom = VRAM_HEIGHT - 1;
        break;
      case RenderCommandType::DrawAreaTopLeft:
      {
        const std::uint32_t packed = static_cast<std::uint32_t>(cmd.args[0]);
        draw_area_left = static_cast<int>(packed & 0x3FFu);
        draw_area_top = static_cast<int>((packed >> 10) & 0x3FFu);
        break;
      }
      case RenderCommandType::DrawAreaBottomRight:
      {
        const std::uint32_t packed = static_cast<std::uint32_t>(cmd.args[0]);
        draw_area_right = static_cast<int>(packed & 0x3FFu);
        draw_area_bottom = static_cast<int>((packed >> 10) & 0x3FFu);
        break;
      }
      case RenderCommandType::DrawMode:
        dither_enabled = (static_cast<std::uint32_t>(cmd.args[0]) & (1u << 9)) != 0;
        break;
      case RenderCommandType::DisplayArea:
      {
        const std::uint32_t packed = static_cast<std::uint32_t>(cmd.args[0]);
        display_x = static_cast<int>(packed & 0x3FFu);
        display_y = static_cast<int>((packed >> 10) & 0x1FFu);
        break;
      }
      case RenderCommandType::DisplayHRange:
      {
        const std::uint32_t packed = static_cast<std::uint32_t>(cmd.args[0]);
        display_h_start = static_cast<int>(packed & 0xFFFu);
        display_h_end = static_cast<int>((packed >> 12) & 0xFFFu);
        int w = (display_h_end - display_h_start) / 8;
        if (w > 0)
        {
          display_w = w;
        }
        break;
      }
      case RenderCommandType::DisplayVRange:
      {
        const std::uint32_t packed = static_cast<std::uint32_t>(cmd.args[0]);
        display_v_start = static_cast<int>(packed & 0x3FFu);
        display_v_end = static_cast<int>((packed >> 10) & 0x3FFu);
        int h = display_v_end - display_v_start;
        if (h > 0)
        {
          display_h = h;
        }
        break;
      }
      case RenderCommandType::DisplayMode:
      {
        const std::uint32_t word = static_cast<std::uint32_t>(cmd.args[0]);
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
      }

      render_pending.pop();
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

extern "C" CAMLprim value renderer_submit_named(value name, value a0, value a1,
                                                value a2, value a3)
{
  CAMLparam5(name, a0, a1, a2, a3);
  {
    std::lock_guard<std::mutex> lock(renderer_mutex);
    if (renderer_instance.get() == nullptr)
    {
      caml_invalid_argument("renderer_submit_named: renderer not initialized");
    }
    renderer_instance->submit_named(String_val(name), Int_val(a0), Int_val(a1),
                                    Int_val(a2), Int_val(a3));
  }
  CAMLreturn(Val_unit);
}

extern "C" CAMLprim value renderer_submit_polygon_flat(value tuple)
{
  CAMLparam1(tuple);
  if (renderer_instance.get() == nullptr)
  {
    caml_invalid_argument("renderer_submit_polygon_flat: renderer not initialized");
  }
  renderer_instance->submit_polygon_flat(Int_val(Field(tuple, 0)),
                                         Int_val(Field(tuple, 1)),
                                         Int_val(Field(tuple, 2)),
                                         Int_val(Field(tuple, 3)),
                                         Int_val(Field(tuple, 4)));
  CAMLreturn(Val_unit);
}

extern "C" CAMLprim value renderer_submit_polygon_shaded(value tuple)
{
  CAMLparam1(tuple);
  if (renderer_instance.get() == nullptr)
  {
    caml_invalid_argument("renderer_submit_polygon_shaded: renderer not initialized");
  }
  renderer_instance->submit_polygon_shaded(
      Int_val(Field(tuple, 0)), Int_val(Field(tuple, 1)),
      Int_val(Field(tuple, 2)), Int_val(Field(tuple, 3)),
      Int_val(Field(tuple, 4)), Int_val(Field(tuple, 5)),
      Int_val(Field(tuple, 6)));
  CAMLreturn(Val_unit);
}

extern "C" CAMLprim value renderer_submit_polygon_flat_quad(value tuple)
{
  CAMLparam1(tuple);
  if (renderer_instance.get() == nullptr)
  {
    caml_invalid_argument(
        "renderer_submit_polygon_flat_quad: renderer not initialized");
  }
  renderer_instance->submit_polygon_flat_quad(
      Int_val(Field(tuple, 0)), Int_val(Field(tuple, 1)),
      Int_val(Field(tuple, 2)), Int_val(Field(tuple, 3)),
      Int_val(Field(tuple, 4)), Int_val(Field(tuple, 5)));
  CAMLreturn(Val_unit);
}

extern "C" CAMLprim value renderer_submit_polygon_shaded_quad(value tuple)
{
  CAMLparam1(tuple);
  if (renderer_instance.get() == nullptr)
  {
    caml_invalid_argument(
        "renderer_submit_polygon_shaded_quad: renderer not initialized");
  }
  renderer_instance->submit_polygon_shaded_quad(
      Int_val(Field(tuple, 0)), Int_val(Field(tuple, 1)),
      Int_val(Field(tuple, 2)), Int_val(Field(tuple, 3)),
      Int_val(Field(tuple, 4)), Int_val(Field(tuple, 5)),
      Int_val(Field(tuple, 6)), Int_val(Field(tuple, 7)),
      Int_val(Field(tuple, 8)));
  CAMLreturn(Val_unit);
}

extern "C" CAMLprim value renderer_submit_draw_area_top_left(value packed_xy)
{
  CAMLparam1(packed_xy);
  if (renderer_instance.get() == nullptr)
  {
    caml_invalid_argument(
        "renderer_submit_draw_area_top_left: renderer not initialized");
  }
  renderer_instance->submit_draw_area_top_left(Int_val(packed_xy));
  CAMLreturn(Val_unit);
}

extern "C" CAMLprim value renderer_submit_draw_area_bottom_right(value packed_xy)
{
  CAMLparam1(packed_xy);
  if (renderer_instance.get() == nullptr)
  {
    caml_invalid_argument(
        "renderer_submit_draw_area_bottom_right: renderer not initialized");
  }
  renderer_instance->submit_draw_area_bottom_right(Int_val(packed_xy));
  CAMLreturn(Val_unit);
}

extern "C" CAMLprim value renderer_submit_draw_mode(value draw_mode)
{
  CAMLparam1(draw_mode);
  if (renderer_instance.get() == nullptr)
  {
    caml_invalid_argument("renderer_submit_draw_mode: renderer not initialized");
  }
  renderer_instance->submit_draw_mode(Int_val(draw_mode));
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
