var uzume = (function(){
var uzume = {};

uzume.World = document.registerElement('uzume-world', {
  prototype: Object.create(HTMLDivElement.prototype, {
  })});

uzume.Layer = document.registerElement('uzume-layer', {
  prototype: Object.create(HTMLDivElement.prototype, {
  })});

uzume.Layer = document.registerElement('uzume-image', {
  prototype: Object.create(HTMLDivElement.prototype, {
  })});

uzume.Layer = document.registerElement('uzume-text', {
  prototype: Object.create(HTMLDivElement.prototype, {
  })});

return uzume;
})();
