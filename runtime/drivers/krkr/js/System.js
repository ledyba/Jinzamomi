jinzamomi.krkr.global.System = (function() {
  var System = {
    /** @type {function[]} */
    continuousHandlers_: [],
    /** @type {Object.<number,string>} */
    messages_: {},
    /**
     * @type {Object.<string, string>}
     */
    args_: {},
    /**
     * @type {string}
     */
    projectBase_: "",
    /**
     * @return {string} ユーザのホームディレクトリのパス
     */
    get appDefaultPath() {
      console.warn("appDefaultPath");
      return "";
    },
    /**
     * @return {string} データ保存場所のパス
     */
    get dataPath() {
      console.warn("dataPath");
      return "";
    },
    /**
     * @return {number} デスクトップ高さ
     */
    get desktopHeight() {
      return 1080;
    },
    /**
     * @return {number} デスクトップ左端位置
     */
    get desktopLeft() {
      return 0;
    },
    /**
     * @return {number} デスクトップ上端位置
     */
    get desktopTop() {
      return 0;
    },
    /**
     * @return {number} デスクトップ幅
     */
    get desktopWidth() {
      return 1920;
    },
    /**
     * @return {number} 描画に使用するスレッドの数
     */
    get drawThreadNum() {
      return 1;
    },
    /**
     *イベント配信が停止されているかどうか
     * @type {boolean}
     */
    eventDisabled: false,
    /**
     *イベント配信が停止されているかどうか
     * @type {boolean}
     */
    eventDisabled: false,
    /**
     * 捕捉されなかった例外のためのハンドラ関数
     * @type {function}
     */
    exceptionHandler: function() {},
    /**
     * @return {string} 吉里吉里本体のパス
     */
    get exeName() {
      console.warn("exeName");
      return "krkr.exe";
    },
    /**
     * @return {string} 吉里吉里本体のあるフォルダのパス
     */
    get exePath() {
      console.warn("exePath");
      return "";
    },
    /**
     * メインウィンドウが閉じたときに終了するかどうか
     * @type {boolean}
     */
    exitOnWindowClose: true,
    /**
     * 画像キャッシュ制限
     * @type {number}
     */
    graphicCacheLimit: 0,
    /**
     * アプリケーションがアクティブになったとき
     * @type {function}
     */
    onActivate: function() {},
    /**
     * アプリケーションが非アクティブになったとき
     * @type {function}
     */
    onDeactivate: function() {},
    /**
     * @return {string} OS 名
     */
    get osName() {
      return "html5";
    },
    /**
     * @return {string} platformName
     */
    get platformName() {
      return "jinzamomi";
    },
    /**
     * @return {string} マイドキュメントのパス
     */
    get personalPath() {
      console.warn("personalPath");
      return "";
    },
    /**
     * @return {number} 画面高さ
     */
    get screenHeight() {
      return 0;
    },
    /**
     * @return {number} 画面幅
     */
    get screenWidth() {
      return 0;
    },
    /**
     * タイトル
     * @type {string}
     */
    title: "krkr",
    /**
     * @return {string} versionInformation
     */
    get versionInformation() {
      return "jinzamomi 1.0";
    },
    /**
     * @return {string} versionString
     */
    get versionString() {
      return "1.0";
    },
    /**
     * Continuous ハンドラの追加
     * @param {function} clbk
     */
    addContinuousHandler: function(clbk) {
      this.continuousHandlers_.push(clbk);
    },
    /**
     * メッセージ割り当ての変更
     * @param {number} id
     * @param {string} msg
     * @return {boolean}
     */
    assignMessage: function(id, msg) {
      if (this.messages_.hasOwnProperty(id)) {
        this.messages_[id] = msg;
        return true;
      }
      return false;
    },
    /**
     * 二重起動のチェック
     * @param {string} key
     * @return {boolean}
     */
    createAppLock: function(key) {
      console.warn("jinzamomi does not support app lock.");
      return false;
    },
    /**
     * UUID 文字列の生成
     * @return {string}
     */
    createUUID: function() {
      console.warn("We don't implement System.createUUID yet.")
      return "e8b2a2b5-5ceb-4f75-a08b-1f1bdfdca4f1";
    },
    /**
     * メモリのコンパクト化
     */
    doCompact: function(level) {
      console.warn("doCompact does nothing today.");
    },
    /**
     * 吉里吉里の同期終了
     */
    exit: function() {
      console.warn("exit does nothing today.");
    },
    /**
     * コマンドラインオプションの取得
     * @param {string} name
     * @return {string|undefined}
     */
    getArgument: function(name) {
      return System.args_[name];
    },
    /**
     * キー状態の取得
     * @param {number} code
     * @return {boolean}
     */
    getKeyState: function() {
      console.error("Not implemented yet");
    },
    /**
     * ティックカウントの取得
     * @return {number}
     */
    getTickCount: function() {
      return new Date().getTime();
    },
    /**
     * ティックカウントの取得
     * @param {string} text
     * @param {string|undefined} caption
     */
    inform: function(text, caption) {
      caption = caption || "<msg>";
      alert("[" + caption + "]" + text);
    },
    /**
     * 文字列の入力
     * @param {string} caption
     * @param {string} prompt
     * @param {string} initString
     * @return {string}
     */
    inputString: function(caption, prompt, initString) {
      return window.prompt("[" + caption + "]" + prompt, initString);
    },
    /**
     * レジストリの読み込み
     * @param {string} key
     * @return {string}
     */
    readRegValue: function() {
      throw new Error("jinzamomi does not support readRegValue");
    },
    /**
     * Continuous ハンドラの削除
     * @param {function} clbk
     */
    removeContinuousHandler: function(clbk) {
      var idx = this.continuousHandlers_.indexOf(clbk);
      if (idx >= 0) {
        this.continuousHandlers_.splice(idx, 1);
      } else {
        console.warn("Continuous Handler not found.");
      }
    },
    /**
     * コマンドラインオプションの設定
     * @param {string} name
     * @param {string} value
     */
    setArgument: function(name, value) {
      this.args_[name] = value;
    },
    /**
     * ファイル/プログラムの実行
     * @param {string} target
     * @param {string|undefined} param
     * @return {bool}
     */
    shellExecute: function(target, param) {
      console.error("We can't exec shell in browsers.");
      return false;
    },
    /**
     * 吉里吉里の非同期終了
     */
    terminate: function() {
      console.warn("terminate does nothing today.");
    },
    /**
     * 色定数の実際の色の取得
     * @param {number} color
     * @return {number}
     */
    toActualColor: function(color) {
      return color;
    },

    /**
     * 画像のキャッシュへの読み込み
     * @param {string[]} storages
     * @param {number|undefined} limitbytes
     * @param {number|timeout} timeout
     */
    touchImages: function(storages, limitbytes, timeout) {
      limitbytes = limitbytes || 0;
      timeout = timeout || 0;
      console.warn("TODO: Please implement touchImages");
    },
  };



  return System;
})();
