class Test {
  constructor() {
    this.object = {
      a: 5,
      b: this.object.a + 3,
    }
  }
}

const test = new Test();

console.log(test.object);
