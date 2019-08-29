import { Elm } from "./Main.elm";
import logoUrl from "../logo.svg";
import package from "../package.json";

const app = Elm.Main.init({
  node: document.querySelector("main"),
  flags: {
    logoUrl,
    repositoryUrl: package.repository.url
  }
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
image.addEventListener("error", err => {
  app.ports.imageDecoded.send(err.message);
});

app.ports.decodeImage.subscribe(uri => {
  image.src = uri;
});

// vim: set ts=2 sw=2 et:
