jinzamomi.krkr.global.Plugins = (function() {
  var global = jinzamomi.krkr.global;
  var Plugins = {
    /**
     * @return {string[]}
     */
    getList: function() {
    },
    /**
     * link plugin.
     * @param {string} name
     */
    link: function(name) {
      console.error("Jinzamomi currently do not support Plugins.link.");
    },
    /**
     * unlink plugin.
     * @param {string} name
     * @return {boolean}
     */
    unlink: function(storage) {
      console.error("Jinzamomi currently do not support Plugins.unlink");
      return false;
    }
  };
  return Plugins;
})();
