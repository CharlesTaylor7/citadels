/** @type {import('tailwindcss').Config} */
module.exports = {
  plugins: [require("daisyui")],
  content: { 
    files: ["templates/**/*.html", "src/templates.rs"],
  },
  daisyui: {
    themes: ["retro"],
  },
  theme: {
      colors: {
        'suit-royal': "oklch(0.807415% 0.052534 159.095)",
      }
  },
}
