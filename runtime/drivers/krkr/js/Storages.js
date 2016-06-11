jinzamomi.krkr.global.Storages = (function() {
  var global = jinzamomi.krkr.global;
  var paths = [];
  var Storages = {
    /**
     * 自動検索パスへの追加
     * @param {string} path
     */
    addAutoPath: function(path) {
      var listPath = global.System.projectBase_ + path + "/files-list.json";
      global.GetJSON(listPath, function(files) {
        paths.push({path: path, files: files});
      });
    },
    /**
     * ストレージ名の拡張子の切り落とし
     * @param {string} str
     * @return {string}
     */
    chopStorageExt: function(str) {
      var idx = str.lastIndexOf('.',str.indexOf('>'));
      if(idx < 0) {
        return str;
      } else {
        return str.substring(0, idx);
      }
    },
    /**
     * ストレージ名の拡張子の切り落とし
     * @param {string} str
     * @return {string}
     */
    extractStorageExt: function(str) {
      var idx = str.lastIndexOf('.',str.indexOf('>'));
      if(idx < 0) {
        return '';
      } else {
        return str.substring(idx);
      }
    },
    /**
     * ストレージ名の抽出
     * @param {string} str
     * @return {string}
     */
    extractStorageName: function(str) {
      var idx = str.lastIndexOf('/',str.indexOf('>'));
      if(idx < 0) {
        return '';
      } else {
        return str.substring(idx+1);
      }
    },
    /**
     * ストレージ名のパスの抽出
     * @param {string} str
     * @return {string}
     */
    extractStoragePath: function(str) {
      var idx = str.lastIndexOf('/',str.indexOf('>'));
      if(idx < 0) {
        return '';
      } else {
        return str.substring(0, idx);
      }
    },
    /**
     * 完全な統一ストレージ名の取得
     * @param {string} path
     * @return {string}
     */
    getFullPath: function(path) {
      console.error("Storage.getFullPath not implemented.");
    },
    /**
     * ストレージの検索
     * @param {string} storage
     * @return {string}
     */
    getPlacedPath: function(storage) {
      for(var i = 0; i < paths.length; i++) {
        var path = paths[i];
        if(path.files.indexOf(storage) !== -1) {
          return global.System.projectBase_ + path.path + "/" + storage;
        }
      }
      return '';
    },
    /**
     * ストレージの存在確認
     * @param {string} storage
     * @return {boolean}
     */
    isExistentStorage: function(storage) {
      for(var i = 0; i < paths.length; i++) {
        var path = paths[i];
        if(path.files.indexOf(storage) !== -1) {
          return true;
        }
      }
      return '';
    },
    /**
     * 自動検索パスの削除
     * @param {string} path
     */
    removeAutoPath: function(path) {
      for(var i = 0; i < paths.length; i++) {
        if(paths[i].path === path) {
          paths.splice(i,1);
          return;
        }
      }
    },
    /**
     * CD の検索
     * @param {string} path - 検索するCDのボリュームラベルを指定します。
     */
    searchCD: function(volume) {
      console.error("WebブラウザからCDドライブは見えません。");
    },
    /**
     * ファイル選択ダイアログボックスを表示
     * @param {object} params
     */
    selectFile: function(volume) {
      console.error("Storage.selectFile not implemented.");
    }
  };
  return Storages;
})();
