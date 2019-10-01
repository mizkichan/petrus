import { Elm } from "./Main.elm";

const canvas = document.createElement("canvas");
const context = canvas.getContext("2d") as CanvasRenderingContext2D;
const image = new Image();

function main() {
  const app = Elm.Main.init();

  function error(ev: ErrorEvent) {
    app.ports.error.send(ev.error.toString());
  }

  window.addEventListener("error", error);

  function imageDecoded() {
    const { width, height } = image;
    canvas.width = width;
    canvas.height = height;
    context.drawImage(image, 0, 0, width, height);
    const imageData = context.getImageData(0, 0, width, height);
    const data = Array.from(imageData.data);
    app.ports.imageDecoded.send({ width, height, data });
  }

  function decodeImage(uri: string) {
    image.src = uri;
  }

  image.addEventListener("load", imageDecoded);
  app.ports.decodeImage.subscribe(decodeImage);
}

window.addEventListener("DOMContentLoaded", main);

// vim: set ts=2 sw=2 et:
