const addNoteEventListeners = () => {
    const counters = [...document.getElementsByClassName('counter')];
    counters.forEach(counter => {
        const id = counter.textContent;
        const note = document.getElementById(`note${id}`);

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

const addDetailsEventListeners = () => {
    const details = [...document.querySelectorAll("details")]
    details.forEach(d => {
        d.addEventListener("toggle", () => {
            setNotePositions()
        })
    })   
}

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
addDetailsEventListeners();
setNotePositions();

setTimeout(setNotePositions, 100);

window.onresize = setNotePositions;

