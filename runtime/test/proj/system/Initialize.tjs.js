(function(global, scope)
{
  var scope0;
  scope0 = ((Object["create"])(scope));
  {
    scope0["kagVersion"] = ("3.32 stable rev. 2");
  };
  {
    scope0["dm"] = (scope0["Debug"]["message"]);
  };
  scope0["System"]["exceptionHandler"] = function(e)
{
  var scope1;
  scope1 = ((Object["create"])(scope0));
  {
    var scope2;
    scope2 = ((Object["create"])(scope1));
if(scope2["e"] instanceof global["ConductorException"])
    {
      var scope3;
      scope3 = ((Object["create"])(scope2));
      (scope3["Debug"]["logAsError"])((null));
      {
        scope3["event_disabled"] = (scope3["System"]["eventDisabled"]);
      };
      scope3["System"]["eventDisabled"] = scope3["true"];
      (scope3["System"]["inform"])(scope3["e"]["message"]);
      scope3["System"]["eventDisabled"] = scope3["event_disabled"];
      return (scope3["true"]);
    }
    else
    {
      var scope4;
      scope4 = ((Object["create"])(scope2));
      return (scope4["false"]);
    };
  };
};
  scope0["useArchiveIfExists"] = (function(name)
{
  var scope5;
  scope5 = ((Object["create"])(scope0));
  {
    var scope6;
    scope6 = ((Object["create"])(scope5));
    {
      arcname = ((undefined));
    };
if((scope6["Storages"]["isExistentStorage"])(scope6["arcname"] = scope6["System"]["exePath"] + scope6["name"]))
    (scope6["Storages"]["addAutoPath"])(scope6["arcname"] + ">");
;
  };
});
  (scope0["Storages"]["addAutoPath"])(scope0["System"]["exePath"] + "video/");
  (scope0["Storages"]["addAutoPath"])("video/");
  (scope0["Storages"]["addAutoPath"])("others/");
  (scope0["Storages"]["addAutoPath"])("rule/");
  (scope0["Storages"]["addAutoPath"])("sound/");
  (scope0["Storages"]["addAutoPath"])("bgm/");
  (scope0["Storages"]["addAutoPath"])("fgimage/");
  (scope0["Storages"]["addAutoPath"])("bgimage/");
  (scope0["Storages"]["addAutoPath"])("scenario/");
  (scope0["Storages"]["addAutoPath"])("image/");
  (scope0["Storages"]["addAutoPath"])("system/");
  (scope0["useArchiveIfExists"])("video.xp3");
  (scope0["useArchiveIfExists"])("others.xp3");
  (scope0["useArchiveIfExists"])("rule.xp3");
  (scope0["useArchiveIfExists"])("sound.xp3");
  (scope0["useArchiveIfExists"])("bgm.xp3");
  (scope0["useArchiveIfExists"])("fgimage.xp3");
  (scope0["useArchiveIfExists"])("bgimage.xp3");
  (scope0["useArchiveIfExists"])("scenario.xp3");
  (scope0["useArchiveIfExists"])("image.xp3");
  (scope0["useArchiveIfExists"])("system.xp3");
  (scope0["useArchiveIfExists"])("patch.xp3");
  {
    var scope7;
    scope7 = ((Object["create"])(scope0));
    {
      scope7["i"] = (2);
    };
for(; ; (scope7["i"])++)
    {
      var scope8;
      scope8 = ((Object["create"])(scope7));
if((scope8["Storages"]["isExistentStorage"])(scope8["System"]["exePath"] + "patch" + scope8["i"] + ".xp3"))
      (scope8["Storages"]["addAutoPath"])(scope8["System"]["exePath"] + "patch" + scope8["i"] + ".xp3>");
      else
      break;;
    };
  };
  delete(scope0["useArchiveIfExists"]);
  (scope0["Debug"]["notice"])("OS : " + scope0["System"]["osName"] + " (" + scope0["System"]["platformName"] + ")");
  (scope0["Debug"]["notice"])("KAG : " + scope0["kagVersion"]);
  (scope0["Debug"]["notice"])("Kirikiri : " + scope0["System"]["versionString"]);
  {
    scope0["parseStartTick"] = ((scope0["System"]["getTickCount"])((null)));
  };
  scope0["KAGLoadScript"] = (function(name)
{
  var scope9;
  scope9 = ((Object["create"])(scope0));
  {
    var scope10;
    scope10 = ((Object["create"])(scope9));
    {
      scope10["start"] = ((scope10["System"]["getTickCount"])((null)));
    };
    (scope10["Scripts"]["execStorage"])(scope10["name"]);
    (scope10["dm"])(scope10["name"] + " を読み込みました(" + (scope10["System"]["getTickCount"])((null)) - scope10["start"] + "ms)");
  };
});
  {
    scope0["loaded_scripts"] = ((function()
{
  temp0 = ((Object["create"])((null)));
  temp0;
})());
  };
  scope0["KAGLoadScriptOnce"] = (function(name)
{
  var scope11;
  scope11 = ((Object["create"])(scope0));
  {
    var scope12;
    scope12 = ((Object["create"])(scope11));
if(scope12["global"]["loaded_scripts"][scope12["name"]] === scope12["true"])
    scope12["return"];
;
    (scope12["global"]["KAGLoadScript"])(scope12["name"]);
    scope12["global"]["loaded_scripts"][scope12["name"]] = scope12["true"];
  };
});
if((scope0["Storages"]["isExistentStorage"])("Config.tjs"))
  {
    var scope13;
    scope13 = ((Object["create"])(scope0));
    (scope13["KAGLoadScript"])("Config.tjs");
  }
  else
if((scope0["Storages"]["isExistentStorage"])("Config.~new"))
  {
    var scope14;
    scope14 = ((Object["create"])(scope0));
    (scope14["System"]["inform"])("Config.tjs が見つかりません。\nsystem フォルダにある Config.~new ファイルを Config.tjs に改名してください。");
    (scope14["System"]["exit"])((null));
  }
  else
  {
    var scope15;
    scope15 = ((Object["create"])(scope0));
    throw (new((scope15["Exception"])("Config.tjs が見つかりません。")));
  };
if(typeof(scope0["global"]["config_version"]) == "undefined" || scope0["config_version"] != scope0["kagVersion"])
  {
    var scope16;
    scope16 = ((Object["create"])(scope0));
    (scope16["KAGLoadScript"])("UpdateConfig.tjs");
  }
;
if(!((scope0["System"]["createAppLock"])((scope0["System"]["exePath"]["replace"])(new (RegExp)("[^A-Za-z]", "g") , "_"))))
  {
    var scope17;
    scope17 = ((Object["create"])(scope0));
    (scope17["System"]["inform"])(scope17["System"]["title"] + "はすでに起動しています");
    (scope17["System"]["exit"])((null));
  }
;
  {
    var temp1;
    temp1 = ((Object["create"])((null)));
    temp1["get"] = (function()
{
  var scope18;
  scope18 = ((Object["create"])(scope0));
  (scope18["KAGLoadScript"])("YesNoDialog.tjs");
  return (scope18["global"]["askYesNo"]);
});
    (Object["defineProperty"])(scope0, "askYesNo", temp1);
  };
  {
    var temp2;
    temp2 = ((Object["create"])((null)));
    temp2["get"] = (function()
{
  var scope19;
  scope19 = ((Object["create"])(scope0));
  (scope19["KAGLoadScript"])("CheckBoxLayer.tjs");
  return (scope19["global"]["CheckBoxLayer"]);
});
    (Object["defineProperty"])(scope0, "CheckBoxLayer", temp2);
  };
  {
    var temp3;
    temp3 = ((Object["create"])((null)));
    temp3["get"] = (function()
{
  var scope20;
  scope20 = ((Object["create"])(scope0));
  (scope20["KAGLoadScript"])("ButtonLayer.tjs");
  return (scope20["global"]["ButtonLayer"]);
});
    (Object["defineProperty"])(scope0, "ButtonLayer", temp3);
  };
  {
    var temp4;
    temp4 = ((Object["create"])((null)));
    temp4["get"] = (function()
{
  var scope21;
  scope21 = ((Object["create"])(scope0));
  (scope21["KAGLoadScript"])("EditLayer.tjs");
  return (scope21["global"]["EditLayer"]);
});
    (Object["defineProperty"])(scope0, "EditLayer", temp4);
  };
  {
    var temp5;
    temp5 = ((Object["create"])((null)));
    temp5["get"] = (function()
{
  var scope22;
  scope22 = ((Object["create"])(scope0));
  (scope22["KAGLoadScript"])("Plugin.tjs");
  return (scope22["global"]["KAGPlugin"]);
});
    (Object["defineProperty"])(scope0, "KAGPlugin", temp5);
  };
  (scope0["dm"])("KAG System スクリプトを読み込んでいます...");
  (scope0["KAGLoadScript"])("Utils.tjs");
  (scope0["KAGLoadScript"])("KAGLayer.tjs");
  (scope0["KAGLoadScript"])("HistoryLayer.tjs");
  (scope0["KAGLoadScript"])("BGM.tjs");
  (scope0["KAGLoadScript"])("SE.tjs");
  (scope0["KAGLoadScript"])("Movie.tjs");
  (scope0["KAGLoadScript"])("Conductor.tjs");
  (scope0["KAGLoadScript"])("AnimationLayer.tjs");
  (scope0["KAGLoadScript"])("GraphicLayer.tjs");
  (scope0["KAGLoadScript"])("MessageLayer.tjs");
  (scope0["KAGLoadScript"])("Menus.tjs");
  (scope0["KAGLoadScript"])("DefaultMover.tjs");
  (scope0["KAGLoadScript"])("MainWindow.tjs");
if((scope0["Storages"]["isExistentStorage"])("Override.tjs"))
  (scope0["KAGLoadScript"])("Override.tjs");
;
if((scope0["Storages"]["isExistentStorage"])(scope0["System"]["exePath"] + "Override2.tjs"))
  (scope0["KAGLoadScript"])(scope0["System"]["exePath"] + "Override2.tjs");
;
  (scope0["dm"])("スクリプトの読み込みに " + (scope0["System"]["getTickCount"])((null)) - scope0["parseStartTick"] + "ms かかりました");
  scope0["parseStartTick"] = (scope0["System"]["getTickCount"])((null));
  scope0["parseStartTick"] = (scope0["System"]["getTickCount"])((null));
  (typeof(scope0["global"]["kag"]) == "undefined") ? (scope0["global"]["kag"] = new((scope0["KAGWindow"])((null)))) : ((null));
  {
    scope0["f"] = (scope0["kag"]["flags"]);
  };
  {
    scope0["sf"] = (scope0["kag"]["sflags"]);
  };
  {
    scope0["tf"] = (scope0["kag"]["tflags"]);
  };
  {
    var temp6;
    temp6 = ((Object["create"])((null)));
    temp6["get"] = (function()
{
  var scope23;
  scope23 = ((Object["create"])(scope0));
  return (scope23["kag"]["conductor"]["macroParams"]);
});
    (Object["defineProperty"])(scope0, "mp", temp6);
  };
  (scope0["dm"])("KAGMainWindow のコンストラクタで " + (scope0["System"]["getTickCount"])((null)) - scope0["parseStartTick"] + "ms かかりました");
  delete(scope0["parseStartTick"]);
if((scope0["Storages"]["isExistentStorage"])("AfterInit.tjs"))
  (scope0["KAGLoadScript"])("AfterInit.tjs");
;
if((scope0["Storages"]["isExistentStorage"])(scope0["System"]["exePath"] + "AfterInit2.tjs"))
  (scope0["KAGLoadScript"])(scope0["System"]["exePath"] + "AfterInit2.tjs");
;
  {
    var scope24;
    scope24 = ((Object["create"])(scope0));
    {
      scope24["ovr"] = ((scope24["System"]["getArgument"])("-ovr"));
    };
if(scope24["ovr"] !== scope24["void"] & (uzume.krkr.getPropertyDescriptor)(scope24, "ovr") != "yes")
    (scope24["Scripts"]["eval"])(scope24["ovr"]);
;
  };
  (scope0["kag"]["process"])("first.ks");
})(uzume.krkr.global, uzume.krkr.global);