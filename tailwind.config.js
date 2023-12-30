/** @type {import('tailwindcss').Config} */
module.exports = {
  plugins: [require("daisyui")],
  content: { 
    files: ["templates/**/*.html", "src/templates.rs"],
  },
  daisyui: {
    themes: ["dark","retro"],
  },
  theme: {
    extend: {
      colors: {
        'suit-trade': "rgb(104, 189, 126)",
        'suit-military': "var(--fallback-p,oklch(var(--p)/var(--tw-bg-opacity)))",
        'suit-religious': "rgb(75, 203, 214)",
        'suit-royal': "rgb(207, 182, 60)",
        'suit-unique': "rgb(185, 153, 224)",
        
      }
    }
  },
}
