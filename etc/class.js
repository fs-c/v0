class Class {
  constructor() {
    this.token = '123';
  }

  log() {
    console.log(this.token);
  }
}

Class.prototype.info = function() {
  console.log(this.token);
}

Class.prototype.object = {
  prop: () => console.log(this.token),
  funcProp: function() { console.log(this.token); },
}

const instance = new Class();

instance.log();

instance.info();

instance.object.prop();
instance.object.funcProp();
