uzume.krkr.global.Scripts = (function() {
  var global = uzume.krkr.global;
  var Script = {
    /**
     */
    dump: function() {
      log.info("dump()");
    },
    /**
     * 式の評価
     * @param {string} expr
     * @return {?}
     */
    eval: function(expr) {
      console.error("Jinzamomi currently do not support eval.");
    },
    /**
     * ストレージ上の式の評価
     * @param {string} expr
     * @return {?}
     */
    evalStorage: function(storage) {
      console.error("Jinzamomi currently do not support eval.");
    },
    /**
     * スクリプトの実行
     * @param {string} script
     */
    exec: function(script) {
      console.error("Jinzamomi currently do not support dynamic eval.");
    },
    /**
     * ストレージ上のスクリプトの実行
     * @param {string} script
     */
    execStorage: function(storage) {
      console.info("execStorage: ", storage);
      var script = document.createElement("script")
      script.setAttribute("src", global.System.projectBase_ + storage + ".js");
      document.body.appendChild(script);
    },
    /**
     * ストレージ上のスクリプトの実行
     * @param {number} limit
     * @return {string}
     */
    getTraceString: function(limit) {
      return (new Error()).stack;
    }
  };
  return Script;
})();
