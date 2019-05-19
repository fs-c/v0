import Two from 'two.js';

const two = new Two({ fullscreen: true });

const degToRad = (deg) => deg * Math.PI / 180;

const centerScene = (scene) => {
    scene.translation = new Two.Vector(window.innerWidth / 2,
        window.innerHeight / 2);
};

class Hexagon {
    constructor(options = {}) {
        this.opts = Object.assign({
            rotation: 90,
            sideLength: 100,
        }, options);

        this.shape = two.makePolygon(0, 0, this.opts.sideLength, 6);

        this.shape.fill = 'white';
        this.shape.stroke = 'black';
        this.shape.linewidth = 5;
        this.shape.rotation = degToRad(this.opts.rotation);
    }

    get area() {
        return (3 * Math.sqrt(3) * Math.pow(this.sideLength, 2)) / 2;
    }
}

document.addEventListener('DOMContentLoaded', () => {
    const container = document.createElement('div');

    container.id = 'container';
    container.classList.add('fit-screen');

    document.body.appendChild(container);

    two.appendTo(container);

    const h1 = new Hexagon();
    const h2 = new Hexagon({ rotation: 0, sideLength: 250 });

    h1.shape.opacity = 0.5;

    centerScene(two.scene);

    two.update();
});

window.addEventListener('resize', (ev) => {
    centerScene(two.scene);

    two.update();
});
