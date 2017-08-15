'use strict';

import * as Electron from "electron";
import * as path from "path";
import * as url  from "url";

let win : Electron.BrowserWindow ;

function createWindow () : void {
  win = new Electron.BrowserWindow({width: 800, height: 600});

  win.loadURL(url.format({
    pathname: path.join(__dirname, "index.html"),
    protocol: "file",
    slashes: true
  }));

  win.webContents.openDevTools();

  win.on("closed", () => {
    win = null;
  });
}

Electron.app.on("ready", createWindow);

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
