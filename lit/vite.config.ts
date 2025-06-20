import { defineConfig } from "vite";

export default defineConfig({
  plugins: [],
  build: {
    outDir: "../public/lit/",
    emptyOutDir: true,
    rollupOptions: {
      output: {
        entryFileNames: "bundle.js",
      },
    },
  },
  server: {
    open: true,
    port: 3000,
    proxy: {
      "/api": {
        target: "http://localhost:8080",
      },
      "/static": {
        target: "http://localhost:8080",
      },
    },
  },
});
