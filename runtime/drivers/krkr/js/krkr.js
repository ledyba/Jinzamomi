uzume.krkr = (function() {
  var krkr = {};
  /**
   * main function
   * @param {string} path - project path
   * @param {Object.<string, string>} args
   */
  krkr.main = function(path, args) {
    System.projectBase_ = path;
    System.args_ = args;
    Script.execStorage("startup.js");
  };

  return krkr;
})();
