#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <atomic>
#include <cstdio>
#include <cstdlib>
#include <memory>
#include <mutex>
#include <queue>
#include <stdexcept>
#include <thread>
#include <print>
#include <string>

#include <SDL3/SDL.h>
#include <SDL3/SDL_gpu.h>

class Renderer
{
public:
  Renderer()
  {
    init_sdl();
    create_window();
    create_gpu_device();
    std::println("Renderer initialized");
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
  const int WIDTH = 800;
  const int HEIGHT = 600;

  std::queue<int> command_queue;
  std::mutex queue_mutex;
  std::atomic_bool close_requested{false};
  std::atomic_bool stop_requested{false};
  std::thread worker_thread;

  SDL_Window *window = nullptr;
  SDL_GPUDevice *gpu_device = nullptr;

  void throw_sdl_error(const char *message)
  {
    throw std::runtime_error(std::string(message) + ": " + SDL_GetError());
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
  }

  void create_gpu_device()
  {
    gpu_device = SDL_CreateGPUDevice(SDL_GPU_SHADERFORMAT_SPIRV, true, "vulkan");
    if (gpu_device == nullptr)
    {
      throw_sdl_error("failed to create GPU device");
    }

    if (!SDL_ClaimWindowForGPUDevice(gpu_device, window))
    {
      throw_sdl_error("failed to claim window for GPU device");
    }
  }

  void drain_commands()
  {
    std::queue<int> pending;
    {
      std::lock_guard<std::mutex> lock(queue_mutex);
      pending.swap(command_queue);
    }

    while (!pending.empty())
    {
      std::printf("Processing command: %d\n", pending.front());
      pending.pop();
    }
  }

  void thread_loop()
  {
    while (!stop_requested.load() && !close_requested.load())
    {
      SDL_Event event;
      while (SDL_PollEvent(&event))
      {
        if (event.type == SDL_EVENT_QUIT || event.type == SDL_EVENT_WINDOW_CLOSE_REQUESTED)
        {
          close_requested.store(true);
        }
      }

      drain_commands();
      draw_frame();
      SDL_Delay(1);
    }
  }

  void draw_frame()
  {
    SDL_GPUCommandBuffer *commandBuffer = SDL_AcquireGPUCommandBuffer(gpu_device);
    if (commandBuffer == nullptr)
    {
      throw_sdl_error("failed to acquire GPU command buffer");
    }

    SDL_GPUTexture *swapchainTexture = nullptr;
    Uint32 textureWidth = 0;
    Uint32 textureHeight = 0;
    if (!SDL_WaitAndAcquireGPUSwapchainTexture(commandBuffer, window, &swapchainTexture, &textureWidth, &textureHeight))
    {
      SDL_CancelGPUCommandBuffer(commandBuffer);
      throw_sdl_error("failed to acquire swapchain texture");
    }

    if (swapchainTexture == nullptr)
    {
      SDL_CancelGPUCommandBuffer(commandBuffer);
      return;
    }

    SDL_GPUColorTargetInfo colorTargetInfo{};
    colorTargetInfo.texture = swapchainTexture;
    colorTargetInfo.mip_level = 0;
    colorTargetInfo.layer_or_depth_plane = 0;
    colorTargetInfo.clear_color = {1.0f, 0.0f, 0.0f, 1.0f};
    colorTargetInfo.load_op = SDL_GPU_LOADOP_CLEAR;
    colorTargetInfo.store_op = SDL_GPU_STOREOP_STORE;
    colorTargetInfo.resolve_texture = nullptr;
    colorTargetInfo.resolve_mip_level = 0;
    colorTargetInfo.resolve_layer = 0;
    colorTargetInfo.cycle = false;
    colorTargetInfo.cycle_resolve_texture = false;

    SDL_GPURenderPass *renderPass = SDL_BeginGPURenderPass(commandBuffer, &colorTargetInfo, 1, nullptr);
    if (renderPass == nullptr)
    {
      SDL_CancelGPUCommandBuffer(commandBuffer);
      throw_sdl_error("failed to begin GPU render pass");
    }

    SDL_EndGPURenderPass(renderPass);

    if (!SDL_SubmitGPUCommandBuffer(commandBuffer))
    {
      throw_sdl_error("failed to submit GPU command buffer");
    }
  }

  void cleanup()
  {
    if (gpu_device != nullptr)
    {
      SDL_WaitForGPUIdle(gpu_device);
    }

    if (gpu_device != nullptr && window != nullptr)
    {
      SDL_ReleaseWindowFromGPUDevice(gpu_device, window);
    }

    if (gpu_device != nullptr)
    {
      SDL_DestroyGPUDevice(gpu_device);
      gpu_device = nullptr;
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
  CAMLreturn(Val_int(1));
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
