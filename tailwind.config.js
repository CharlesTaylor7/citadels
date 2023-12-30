/** @type {import('tailwindcss').Config} */
module.exports = {
  plugins: [require("daisyui")],
  content: { 
    files: ["templates/**/*.html", "src/templates.rs"],
  },
  daisyui: {
    themes: ["light", "dark", "cupcake", "retro"],
  },
  theme: {
    extend: {
    }
  },
}
