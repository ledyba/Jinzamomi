uzume.krkr = (function(){
var ns = {};
/**
 * main function
 * @param {string} path - project path
 * @param {Object.<string, string>} args
 */
ns.main = function(path, args) {
  ns.System.args_ = args;
};

ns.System = {
  /** @type {function[]} */
  continuousHandlers_: [],
  /** @type {Object.<number,string>} */
  messages_: {}
  /**
   * @type {Object.<string, string>}
   */
  args_: {},
  /**
   * @return {string} ユーザのホームディレクトリのパス
   */
  get appDefaultPath() {
    log.warn("appDefaultPath");
    return "";
  },
  /**
   * @return {string} データ保存場所のパス
   */
  get dataPath() {
    log.warn("dataPath");
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
  get drawThreadNum () {
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
  exceptionHandler: funtion () {},
  /**
   * @return {string} 吉里吉里本体のパス
   */
  get exeName() {
    log.warn("exeName");
    return "krkr.exe";
  },
  /**
   * @return {string} 吉里吉里本体のあるフォルダのパス
   */
  get exePath() {
    log.warn("exePath");
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
  onActivate: funtion() {},
   /**
    * アプリケーションが非アクティブになったとき
    * @type {function}
    */
  onDeactivate: funtion() {},
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
    return "uzume";
  },
  /**
   * @return {string} マイドキュメントのパス
   */
  get personalPath() {
    log.warn("personalPath");
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
    return "Uzume 1.0";
  },
  /**
   * @return {string} versionString
   */
  get versionString() {
    return "1.0";
  },
};
 /**
  * Continuous ハンドラの追加
  * @param {function} clbk
  */
ns.Sysrem.addContinuousHandler = function(clbk) {
  this.continuousHandlers_.push(clbk);
};

/**
 * メッセージ割り当ての変更
 * @param {number} id
 * @param {string} msg
 * @return {boolean}
 */
ns.System.assignMessage = function(id, msg){
  if (this.messages_.hasOwnProperty(id)){
    this.messages_[id] = msg;
    return true;
  }
  return false;
};

/**
 * 二重起動のチェック
 * @param {string} key
 * @return {boolean}
 */
ns.System.createAppLock = function(key) {
  log.warn("Uzume does not support app lock.");
  return false;
};

/**
 * UUID 文字列の生成
 * @return {string}
 */
ns.System.createUUID = function() {
  log.warn("We don't implement System.createUUID yet.")
  return "e8b2a2b5-5ceb-4f75-a08b-1f1bdfdca4f1";
}

/**
 * メモリのコンパクト化
 */
ns.System.doCompact = function(level) {
  log.warn("doCompact does nothing today.");
};

/**
 * 吉里吉里の同期終了
 */
ns.System.exit = function() {
  log.warn("exit does nothing today.");
};

/**
 * コマンドラインオプションの取得
 * @param {string} name
 * @return {string|undefined}
 */
ns.System.getArgument = function(name) {
  return ns.System.args_[name];
};

/**
 * キー状態の取得
 * @param {number} code
 * @return {boolean}
 */
ns.System.getKeyState = function() {

};

/**
 * ティックカウントの取得
 * @return {number}
 */
ns.System.getTickCount = function() {
  return new Date().getTime();
};

/**
 * ティックカウントの取得
 * @param {string} text
 * @param {string|undefined} caption
 */
ns.Sytem.inform = function(text,caption) {
  caption = caption || "<msg>";
  alert("["+caption+"]"+text);
};

/**
 * 文字列の入力
 * @param {string} caption
 * @param {string} prompt
 * @param {string} initString
 * @return {string}
 */
ns.System.inputString = function(caption, prompt, initString) {
  return window.prompt("["+caption+"]"+prompt, initString);
};

/**
 * レジストリの読み込み
 * @param {string} key
 * @return {string}
 */
ns.System.readRegValue = function() {
  throw new Error("Uzume does not support readRegValue");
};

/**
 * Continuous ハンドラの削除
 * @param {function} clbk
 */
ns.System.removeContinuousHandler = function(clbk){
  var idx = this.continuousHandlers_.indexOf(clbk);
  if (idx >= 0){
    this.continuousHandlers_.splice(idx, 1);
  }else{
    log.warn("Continuous Handler not found.");
  }
};

/**
 * コマンドラインオプションの設定
 * @param {string} name
 * @param {string} value
 */
ns.Sytem.setArgument = function(name, value) {
  this.args_[name] = value;
};

/**
 * ファイル/プログラムの実行
 * @param {string} target
 * @param {string|undefined} param
 * @return {bool}
 */
ns.System.shellExecute = function(target, param) {
  log.error("We can't exec shell in browsers.");
  return false;
};

/**
 * 吉里吉里の非同期終了
 */
ns.System.terminate = function(){
  log.warn("terminate does nothing today.");
};

/**
 * 色定数の実際の色の取得
 * @param {number} color
 * @return {number}
 */
ns.System.toActualColor = function(color) {
  return color;
};

/**
 * 画像のキャッシュへの読み込み
 * @param {string[]} storages
 * @param {number|undefined} limitbytes
 * @param {number|timeout} timeout
 */
ns.System.touchImages = function(storages, limitbytes, timeout){
  limitbytes = limitbytes || 0;
  timeout = timeout || 0;
  log.warn("TODO: Please implement touchImages");
};

return ns;
})();
