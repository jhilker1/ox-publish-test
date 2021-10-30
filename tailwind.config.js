const colors = require('tailwindcss/colors')
const defaultTheme = require('tailwindcss/defaultTheme')
const plugin = require('tailwindcss/plugin')



module.exports = {
  mode: 'jit',
  purge: [
    "public/**/*.html", 
    "org/**/*.org"
  ],
  darkMode: 'class', // or 'media' or 'class'
  theme: {
    gridTemplateAreas: {
      mobile: ['header header header', 'main main main', 'footer footer footer'],
      desktop: ['sidebar header header', 'sidebar main main', 'sidebar footer footer'],
    },
    extend: {
      gridTemplateColumns: {
        layout: '0.7fr 2.3fr 1fr',
      },
      gridTemplateRows: {
        layout: '0.2fr 2.6fr 0.2fr',
      },
      height: {
        '13': '52px',
        '18': '66px'
      },
      colors: {
        blueGray: colors.blueGray,
         'royal': {
          DEFAULT: '#2E8BC0',
          '50': '#DDEDF7',
          '100': '#C8E3F2',
          '200': '#9FCDE8',
          '300': '#76B8DE',
          '400': '#4DA3D4',
          '500': '#2E8BC0',
          '600': '#246D97',
          '700': '#1A4F6E',
          '800': '#103245',
          '900': '#07141B'
        },
        'brown': {  
          DEFAULT: '#A67C47',  
          '50': '#F2EAE1',
          '100': '#EADECF',  
          '200': '#DBC6AB',
          '300': '#CCAD87',  
          '400': '#BC9564',  
          '500': '#A67C47',  
          '600': '#826138',  
          '700': '#5F4728',  
          '800': '#3B2C19',  
          '900': '#17110A'},

          'navy': {  DEFAULT: '#394855',  '50': '#758DA3',  '100': '#6C869C',  '200': '#5E778C',  '300': '#52677A',  '400': '#455867',  '500': '#394855',  '600': '#313E49',  '700': '#29333D',  '800': '#202930',  '900': '#181F24'},
          'bismark': {  DEFAULT: '#3D627D',  '50': '#A1BDD1',  '100': '#93B3CA',  '200': '#78A0BD',  '300': '#5C8CB0',  '400': '#4A7798',  '500': '#3D627D',  '600': '#304D62',  '700': '#223746',  '800': '#15222B',  '900': '#070C0F'},
      },
      typography(theme) {
        return {
          sm: {
            css: {
               'ul > li': {
                position: 'relative',
              },
              'ul > li::before': {
                content: '""',
                position: 'absolute',
                backgroundColor: theme('colors.gray.500', defaultTheme.colors.gray[500]),
                borderRadius: '50%',
              },
              'ol > li': {
                position: 'relative',
              },
              'ol > li::before': {
                content: 'counter(list-item, var(--list-counter-style, decimal)) "."',
                position: 'absolute',
                fontWeight: '400',
                color: theme('colors.gray.700', defaultTheme.colors.gray[700]),
              },
            }
          },
          dark: {
            css: {
              color: theme('colors.gray.300'),
          '[class~="lead"]': {
            color: theme('colors.gray.300')
          },
          a: {
            color: theme('colors.royal.400'),
            '&:hover': {
              color: theme('colors.royal.300')
            }
          },
          strong: {
            color: theme('colors.gray.100')
          },

          'ol > li::before': {
            color: theme('colors.gray.400')
          },
          'ul > li::before': {
            backgroundColor: theme('colors.gray.600')
          },
          hr: {
            borderColor: theme('colors.gray.700')
          },
          blockquote: {
            color: theme('colors.gray.100'),
            borderLeftColor: theme('colors.gray.700')
          },
          h1: {
            color: theme('colors.gray.100')
          },
          h2: {
            color: theme('colors.gray.100'),
            borderBottomColor: theme('colors.gray.800')
          },
          h3: {
            color: theme('colors.gray.100'),
            borderBottomColor: theme('colors.gray.800')
          },
          h4: {
            color: theme('colors.gray.100')
          },
          'figure figcaption': {
            color: theme('colors.gray.400')
          },
          code: {
            color: theme('colors.gray.100')
          },
          pre: {
            color: theme('colors.gray.700'),
          },
          thead: {
            color: theme('colors.gray.100'),
            borderBottomColor: theme('colors.gray.600')
          },
          'tbody tr': {
            borderBottomColor: theme('colors.gray.700')
          }
            }
          }
        }
      },
      screens: {
        mobile: { max: '1023px'}
      },
      outline: {
        red: ['2px solid red', '0px'],
        gray: ['2px solid gray', '2px']
      },
      fontFamily: {
        mono: ['"Iosevka Mono Web"', defaultTheme.fontFamily.mono],
        serif: ['"Iosevka Slab Web"', defaultTheme.fontFamily.serif],
        sans: ['"Iosevka Sans Web"', defaultTheme.fontFamily.sans]
      }
    },
  },
  variants: {
    extend: {
        borderColor: ['active'],
        borderWidth: ['active']
    },
  },
  plugins: [
    require('@savvywombat/tailwindcss-grid-areas'),
    require('@tailwindcss/typography')({
      className: 'org'
    }),
    require('tailwind-scrollbar'),
    plugin(function({ addUtilities }) {
      const scrollUtilities = {
        '.scroll-smooth': {
          scrollBehavior: 'smooth',
        },
        '.scroll-auto': {
          scrollBehavior: 'auto',
        },
      }

      addUtilities(scrollUtilities)
    }),
  ],
}
