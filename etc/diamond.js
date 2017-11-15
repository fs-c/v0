// const dia = (w = 7, s = '') => for (let i = 0; i < w * 2; i++) s += i === w ? '\n' :
const dia = (w = 7) => {for (let i = 1; i <= Math.pow(w, 2); i++) console.log(Math.ceil(i / w))}

dia()
