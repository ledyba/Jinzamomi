uzume.krkr = (function() {
  var krkr = {};
  var global = {};
  krkr.global = global;
  /**
   * main function
   * @param {string} path - project path
   * @param {Object.<string, string>} args
   */
  krkr.main = function(path, args) {
    global.System.projectBase_ = path;
    global.System.args_ = args;
    global.Scripts.execStorage("startup.tjs");
  };
  global.Window = {};
  /**
   * getPropertyDescriptor
   * @param {Object} obj
   * @param {string} name
   * @return {Object}
   */
  krkr.getPropertyDescriptor = function(obj, name) {
    if(!(name in obj)){
      throw Error(name + " not found in "+obj);
    }
    for(;!obj.hasOwnProperty(name);obj = obj.__proto__);
    return Object.getOwnPropertyDescriptor(obj, name);
  };

  return krkr;
})();
