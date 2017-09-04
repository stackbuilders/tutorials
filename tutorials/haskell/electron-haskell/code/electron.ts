'use strict';

import * as Electron from "electron";
import { ChildProcess, spawn }  from "child_process";
import * as path from "path";
import * as url  from "url";

let win : Electron.BrowserWindow ;
let apiServer : ChildProcess;

function createWindow () : void {
  win = new Electron.BrowserWindow({width: 800, height: 600});

  win.loadURL(url.format({
    pathname: path.join(__dirname, "index.html"),
    protocol: "file",
    slashes: true
  }));

  win.on("closed", () => {
    win = null;
  });
}

function startHaskellServer () : ChildProcess {
  return apiServer = spawn(path.join(__dirname, "bin/electron-haskell-exe"));
}

Electron.app.on("ready", createWindow);
Electron.app.on("ready", startHaskellServer);

Electron.app.on('will-quit', function() {
  apiServer.kill()
})

Electron.app.on("window-all-closed", () => {
  if (process.platform !== "darwin") {
    Electron.app.quit();
  }
});

Electron.app.on("activate", () => {
  if (win === null) {
    createWindow();
  }
});
