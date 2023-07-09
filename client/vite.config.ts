import { defineConfig, ServerOptions } from "vite";
import elmPlugin from "vite-plugin-elm";

const server: ServerOptions = {
  host: "0.0.0.0", // Allow clients from local network
  port: 8080,
  proxy: {
    "/api": {
      target: "http://0.0.0.0:8081", // Replace with your API server URL
      rewrite: (path) => path.replace(/^\/api/, ""),
    },
  },
};

export default defineConfig(({ command }) => {
  if (command === "build") {
    // Production settings
    return {
      plugins: [elmPlugin({ debug: false, optimize: true })],
    };
  } else {
    // Dev settings
    // 'debug' = enable debugger and show debugger UI
    return {
      plugins: [elmPlugin({ debug: false, optimize: false })],
      server,
    };
  }
});
