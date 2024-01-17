/** @type {import('tailwindcss').Config} */
module.exports = {
  plugins: [require("daisyui")],
  content: { 
    files: ["templates/**/*", "src/templates.rs",  "src/templates/filters.rs"],
  },
  daisyui: {
    themes: ["dark","retro"],
  },
  theme: {
    extend: {
       backgroundImage: {
        'gradient-radial': 'radial-gradient(var(--tw-gradient-stops))',
      },
      colors: {
        'suit-trade': "rgb(104, 189, 126)",
        'suit-military': "var(--fallback-p,oklch(var(--p)/var(--tw-bg-opacity)))",
        'suit-religious': "rgb(75, 203, 214)",
        'suit-noble': "rgb(207, 182, 60)",
        'suit-unique': "rgb(185, 153, 224)",
        
      }
    }
  },
}
