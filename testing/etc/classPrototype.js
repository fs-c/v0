class Test {
  constructor() {
    this.string = 'owo';
  }
}

Test.prototype.bla = function() {
  console.log(this);
}

const test = new Test();

test.bla();