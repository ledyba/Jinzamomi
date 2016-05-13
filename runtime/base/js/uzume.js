(function(){
var ns = {};

ns.World = document.registerElement('uzume-world', {
  prototype: Object.create(HTMLDivElement.prototype, {
  }
});

ns.Layer = document.registerElement('uzume-layer', {
  prototype: Object.create(HTMLDivElement.prototype, {
  }});

ns.Layer = document.registerElement('uzume-image', {
  prototype: Object.create(HTMLDivElement.prototype, {
  }});

ns.Layer = document.registerElement('uzume-text', {
  prototype: Object.create(HTMLDivElement.prototype, {
  }});

})();
