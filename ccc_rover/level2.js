/*
 * Arc length formula source: https://www.1728.org/radians.htm
 */

const strings = [
	"1.00 1.00 30.00",
	"2.13 4.30 23.00",
	"1.75 3.14 -23.00",
	"2.70 45.00 -34.00",
	"4.20 -5.30 20.00",
	"9.53 8.12 0.00",
];

const degToRad = (deg) => deg * (Math.PI / 180);
const radToDeg = (rad) => rad * (180 / Math.PI);

const solve = (wheelBase, distance, steeringAngle) => {
	const radius = wheelBase / Math.sin(degToRad(steeringAngle));
	const angle = radToDeg(distance / radius);

	const x = radius * Math.cos(degToRad(angle)) * -1 + radius;
	const y = radius * Math.sin(degToRad(angle));

	console.log(`[ r=${radius}, a=${angle} ]: ${x}/${y}`);

	console.log(`${x} ${y} ${angle}\n`);
};

strings.map((e) => solve(...(e.split(' ').map((f) => parseFloat(f)))));