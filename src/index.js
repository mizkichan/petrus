import { Elm } from "./Main.elm";

const app = Elm.Main.init({ node: document.querySelector("main") });

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
      const data = [];
      for (let y = 0; y < height; ++y) {
        for (let x = 0; x < width; ++x) {
          const [r, g, b] = imageData.data.slice((x + y * width) * 4);
          data.push({ x, y, r, g, b });
        }
      }
      app.ports.imageDecoded.send({ width, height, data });
    })
    .catch(err => app.ports.imageDecoded.send(err.message));
});

// vim: set ts=2 sw=2 et:
