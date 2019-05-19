import SVG from 'svg.js';
import './styles.less';

class Hexagon {
    constructor(doc, opts) {
        this.draw = doc;
        this.poly = null;
        this.points = [];

        const options = this.options = Object.assign({
            x: 0, y: 0, centered: false,
            size: 10,
        }, opts);

        console.log(options);

        this.size = options.size;

        if (!options.centered) {
            this.x = options.x;
            this.y = options.y;
        } else {
            this.centerCoordinates();

            window.addEventListener('resize', () => {
                this.centerCoordinates();

                this.update();
            });
        }

        this.update();
    }

    centerCoordinates() {
        this.x = window.innerWidth / 2;
        this.y = window.innerHeight / 2;

        console.log('centering', this.x, this.y);
    }

    calculatePoints(radius, height) {
        const points = [];

        points[0] = [ -1 * radius / 2, -1 * height ];
        points[1] = [ radius / 2, -1 * height ];
        points[2] = [ radius, 0 ];
        points[3] = [ radius / 2, height ];
        points[4] = [ -1 * radius / 2, height ];
        points[5] = [ -1 * radius, 0 ];

        return points;
    }

    update() {
        const radius = this.radius = window.innerHeight / 100 * (this.size / 10);
        const height = (Math.sqrt(3) / 2) * radius;

        const points = new SVG.PointArray(this.calculatePoints(radius, height));

        if (!this.poly) {
            this.poly = this.draw.polygon(points);
        } else {
            this.poly.plot(points);
        }

        this.poly.center(this.x, this.y).transform({ rotation: 0 })
            .fill({ opacity: 0 }).stroke({ color: '#f06', width: 2.5 });

        if (this.options.text) {
            if (!this.text) {
                this.text = this.draw.text(this.options.text);
            } else {
                this.text.text(this.options.text);
            }

            this.text.font({
                style: 'bold',
                anchor: 'left',
                size: this.radius / 3,
            }).move(this.x - this.text.x() / 2, this.y - this.text.y() / 2);
        }
    }
}

document.addEventListener('DOMContentLoaded', () => {
    const container = document.createElement('div');

    container.id = 'container';
    container.classList.add('fit-screen');

    document.body.appendChild(container);

    const draw = SVG('container').size('100%', '100%');

    const hex = new Hexagon(draw, { centered: true, size: 100, text: 'HEXA\nNET' });
});
