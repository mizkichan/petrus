import { Elm } from "./Main.elm";
import logo from "../logo.svg";

const app = Elm.Main.init({
  node: document.querySelector("main"),
  flags: logo
});

app.ports.decodeImage.subscribe(uri => {
  const canvas = document.createElement("canvas");
  const context = canvas.getContext("2d");
  const image = new Image();
  image.src = uri;
  image
    .decode()
    .then(() => {
      const { width, height } = image;
      canvas.width = width;
      canvas.height = height;
      context.drawImage(image, 0, 0, width, height);
      const imageData = context.getImageData(0, 0, width, height);
      const data = Array.from(imageData.data);
      app.ports.imageDecoded.send({ width, height, data });
    })
    .catch(err => app.ports.imageDecoded.send(err.message));
});

// vim: set ts=2 sw=2 et:
