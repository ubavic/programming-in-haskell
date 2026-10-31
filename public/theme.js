const themeKey = 'theme';
const darkTheme = document.getElementById('darkTheme');
const systemMedia = darkTheme.media;

const applyTheme = theme => {
    if (theme === 'dark') {
        darkTheme.media = 'screen';
    } else if (theme === 'light') {
        darkTheme.media = 'not all';
    } else {
        darkTheme.media = systemMedia;
    }
};

const addThemeSwitcher = () => {
    const switcher = document.getElementById('themeSwitcher');
    const buttons = [...switcher.querySelectorAll('button')];

    const markSelected = theme => {
        buttons.forEach(button => {
            button.setAttribute('aria-pressed', button.dataset.theme === (theme ?? ''));
        });
    };

    buttons.forEach(button => {
        button.addEventListener('click', () => {
            const theme = button.dataset.theme;
            if (theme) {
                localStorage.setItem(themeKey, theme);
            } else {
                localStorage.removeItem(themeKey);
            }
            applyTheme(theme);
            markSelected(theme);
        });
    });

    markSelected(localStorage.getItem(themeKey));
    switcher.hidden = false;
};

applyTheme(localStorage.getItem(themeKey));
document.addEventListener('DOMContentLoaded', addThemeSwitcher);
