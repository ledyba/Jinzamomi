uzume.krkr.global.Console = (function(){
  var global = uzume.krkr.global;
  var Console = {
    visible: false
  };
  return Console;
})();
uzume.krkr.global.Controller = (function(){
  var global = uzume.krkr.global;
  var Controller = {
    visible: false
  };
  return Controller;
})();
uzume.krkr.global.Debug = (function() {
  var global = uzume.krkr.global;
  var Script = {
    /**
     */
    logAsError: function() {
      console.error(new Error());
    },
    /**
     * コンソールへメッセージを出力
     * @param {string} message
     */
    message: function(message) {
      console.log(message);
    },
    /**
     * コンソールへ重要なメッセージを出力
     * @param {string} message
     */
    notice: function(message) {
      console.info(message);
    },
    /**
     * コンソールのログの出力開始
     * @param {boolean|undefined} clear
     */
    startLogToFile: function(clear) {
    },
    /**
     * エラー発生時にコンソールのログファイルをクリアするかどうか
     * @type {boolean}
     */
    clearLogFileOnError: false,
    /**
     * @private {uzume.krkr.global.Console}
     */
    console_: Object.create(global.Console),
    /**
     * コンソールオブジェクト
     * @return {uzume.krkr.global.Console}
     */
    get console () {
      return global.Debug.console_;
    },
    /**
     * @private {uzume.krkr.global.Controller}
     */
    controller_: Object.create(global.Controller),
    /**
     * コントローラオブジェクト
     * @return {uzume.krkr.global.Controller}
     */
    get controller () {
      return global.Debug.controller_;
    },
    /**
     * ログファイルの出力先
     * @type {string}
     */
    logLocation: "",
    /**
     * エラー発生時にコンソールのログをファイルに出力するか
     * @type {string}
     */
    logToFileOnError: true
  };
  return Script;
})();
