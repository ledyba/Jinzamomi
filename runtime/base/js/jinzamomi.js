var jinzamomi = (function() {
  var jinzamomi = {};

  jinzamomi.World = document.registerElement('jinzamomi-world', {
    prototype: Object.create(HTMLDivElement.prototype, {})
  });

  jinzamomi.Layer = document.registerElement('jinzamomi-layer', {
    prototype: Object.create(HTMLDivElement.prototype, {})
  });

  jinzamomi.Layer = document.registerElement('jinzamomi-image', {
    prototype: Object.create(HTMLDivElement.prototype, {})
  });

  jinzamomi.Layer = document.registerElement('jinzamomi-text', {
    prototype: Object.create(HTMLDivElement.prototype, {})
  });

  return jinzamomi;
})();
