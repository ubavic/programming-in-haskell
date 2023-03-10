const addNoteEventListeners = () => {
    const counters = [...document.getElementsByClassName('counter')];
    counters.forEach(counter => {
        const id = counter.textContent;
        const note = document.getElementById(`note${id}`);

        note.addEventListener('mouseover', _ => counter.style.color = '#a22');
        note.addEventListener('mouseleave', _ => counter.style.color = 'initial');

        counter.addEventListener('mouseover', _ => note.childNodes[0].style.color = '#a22');
        counter.addEventListener('mouseleave', _ => note.childNodes[0].style.color = '');

        counter.addEventListener('click', _ => {
            if (window.innerWidth < 750) {
                note.style.display = 'block';
            }
        });

        note.addEventListener('click', _ => {
            if (window.innerWidth < 750) {
                note.style.display = '';
            }
        });
    });
};

const setNotePositions = () => {
    let lastNoteEnd = 0;

    const counters = [...document.getElementsByClassName('counter')];
    counters.forEach(counter => {
        const id = counter.textContent;
        const note = document.getElementById(`note${id}`);

        const { top } = counter.getBoundingClientRect();
        const notePosition = top + window.scrollY;

        note.style.top = `${Math.max(notePosition, lastNoteEnd)}px`;

        lastNoteEnd = Math.max(notePosition, lastNoteEnd) + note.getBoundingClientRect().height + 10;
    });
};

addNoteEventListeners();
setNotePositions();

setTimeout(setNotePositions, 2000);

window.onresize = setNotePositions;

