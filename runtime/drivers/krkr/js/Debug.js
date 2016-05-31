jinzamomi.krkr.global.Console = (function(){
  var global = jinzamomi.krkr.global;
  var Console = {
    visible: false
  };
  return Console;
})();
jinzamomi.krkr.global.Controller = (function(){
  var global = jinzamomi.krkr.global;
  var Controller = {
    visible: false
  };
  return Controller;
})();
jinzamomi.krkr.global.Debug = (function() {
  var global = jinzamomi.krkr.global;
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
     * @private {jinzamomi.krkr.global.Console}
     */
    console_: Object.create(global.Console),
    /**
     * コンソールオブジェクト
     * @return {jinzamomi.krkr.global.Console}
     */
    get console () {
      return global.Debug.console_;
    },
    /**
     * @private {jinzamomi.krkr.global.Controller}
     */
    controller_: Object.create(global.Controller),
    /**
     * コントローラオブジェクト
     * @return {jinzamomi.krkr.global.Controller}
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
