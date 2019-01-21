// ==UserScript==
// @name         rrl-hide
// @namespace    https://fsoc.space
// @version      0.1
// @description  allows hiding of fictions, reducing their visibility
// @author       fsoc
// @license      MIT
// @copyright    2019, lw (https://fsoc.space)
// @match        https://www.royalroad.com/fictions/*
// @grant        none
// ==/UserScript==

// ==OpenUserJS==
// @author fsoc
// ==/OpenUserJS==

(function() {

const { localStorage } = window;

const styles = {
    hidden: 'display: none',
    visible: 'display: block',
};

const setIgnoreStyle = (fiction, style) => {
    const cover = fiction.getElementsByTagName('figure').item(0);

    const divs = fiction.getElementsByTagName('div');
    const tags = divs.item(1), stats = divs.item(2);

    [ cover, tags, stats ].forEach((el) => el.setAttribute('style', style));
};

const fictions = document.getElementsByClassName('fiction-list-item');

for (const fiction of fictions) {
    const hideLink = document.createElement('a');

    hideLink.insertAdjacentText('afterbegin', 'Hide');
    hideLink.setAttribute('class', 'font-red-sunglo');

    const id = "ign_" + fiction.getElementsByClassName('fiction-title').item(0)
        .firstElementChild.getAttribute('href').split('/')[2];

    hideLink.onclick = () => {
        const status = localStorage.getItem(id) === "true";
        localStorage.setItem(id, !status);

        setIgnoreStyle(fiction, status ? styles.visible : styles.hidden);
    };

    const container = document.createElement('small');
    container.append(hideLink);

    const heading = fiction.getElementsByClassName('fiction-title').item(0);
    heading.append(container);

    if (localStorage.getItem(id) === "true") {
        setIgnoreStyle(fiction, styles.hidden);
    }
}

})();