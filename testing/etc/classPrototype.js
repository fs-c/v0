class Test {
  constructor() {
    this.string = 'owo';

    this.bla();
  }
}

Test.prototype.bla = function() {
  console.log(this);
}

const test = new Test();