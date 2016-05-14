var Script = (function() {
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
    eval: function(expr) {},
    /**
     * ストレージ上の式の評価
     * @param {string} expr
     * @return {?}
     */
    evalStorage: function(storage) {

    },
    /**
     * スクリプトの実行
     * @param {string} script
     */
    exec: function(script) {

    },
    /**
     * ストレージ上のスクリプトの実行
     * @param {string} script
     */
    execStorage: function(storage) {
      var script = document.createElement("script")
      script.setAttribute("src", System.projectBase_ + storage);
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
