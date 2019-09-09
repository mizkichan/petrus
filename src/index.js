import { Elm } from "./Main.elm";
import package from "../package.json";

const app = Elm.Main.init({
  flags: {
    repositoryUrl: package.repository.url,
    title: `${package.name} - ${package.description}`
  }
});

const send = (portName, object) => {
  try {
    app.ports[portName].send(object);
  } catch (e) {
    app.ports.error.send(e.toString());
    throw e;
  }
};

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
  send("imageDecoded", { width, height, data });
});

image.addEventListener("error", err => {
  app.ports.imageDecoded.send(err.message);
});

app.ports.decodeImage.subscribe(uri => {
  image.src = uri;
});

// vim: set ts=2 sw=2 et:
