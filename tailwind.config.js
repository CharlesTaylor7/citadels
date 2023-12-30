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
      colors: {
        'suit-religious': "var(--fallback-s,oklch(var(--s)/var(--tw-bg-opacity)))",
        'suit-military': "var(--fallback-p,oklch(var(--p)/var(--tw-bg-opacity)))",
      }
  },
}
