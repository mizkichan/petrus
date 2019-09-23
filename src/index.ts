import { Elm } from "./Main.elm";
import packageJson from "../package.json";

const canvas = document.createElement("canvas");
const context = canvas.getContext("2d") as CanvasRenderingContext2D;
const image = new Image();

function main() {
  const app = Elm.Main.init({
    flags: {
      repositoryUrl: packageJson.repository.url,
      title: `${packageJson.name} - ${packageJson.description}`
    }
  });

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

  app.ports.decodeImage.subscribe(decodeImage);
  image.addEventListener("load", imageDecoded);
}

window.addEventListener("DOMContentLoaded", main);

// vim: set ts=2 sw=2 et: