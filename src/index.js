import { Elm } from "./Main.elm";
import package from "../package.json";

const app = Elm.Main.init({
  flags: {
    repositoryUrl: package.repository.url,
    title: `${package.name} - ${package.description}`
  }
});

window.addEventListener("error", ev => {
  app.ports.error.send(ev.error.toString());
});

const canvas = document.createElement("canvas");
const context = canvas.getContext("2d");
const image = new Image();

image.addEventListener("load", () => {
  const { width, height } = image;
  canvas.width = width;
  canvas.height = height;
  context.drawImage(image, 0, 0, width, height);
  const imageData = context.getImageData(0, 0, width, height);
  const data = Array.from(imageData.data);
  app.ports.imageDecoded.send({ width, height, data });
});

app.ports.decodeImage.subscribe(uri => {
  image.src = uri;
});

// vim: set ts=2 sw=2 et:
