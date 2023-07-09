// @ts-ignore
import { Elm } from "./src/Main.elm";
import { displayErrorMessage } from "./errors";

// @ts-ignore
if (process.env.NODE_ENV === "development") {
  const ElmDebugTransform = await import("elm-debug-transformer");

  ElmDebugTransform.register({
    simple_mode: true,
  });
}

const getConfig = async () => {
  const res = await fetch("/nocache/config.json").catch((e) => {
    throw new Error(`Failed to load config.json: ${e.message}`);
  });
  const config = await res.json().catch((_) => {
    throw new Error("Failed to decode config.json: Invalid JSON syntax");
  });
  return config;
};

(async () => {
  const config = await getConfig();
  const rootNode = document.querySelector("#app div") as HTMLDivElement;
  const app = Elm.Main.init({
    flags: {
      windowSize: {
        height: window.innerHeight,
        width: window.innerWidth,
      },
      config,
    },
    node: rootNode,
  });

  app.ports.startDemo.subscribe(function () {
    // Create your WebSocket.

    const socket = new WebSocket(`ws://${location.host}:8081/demo`);

    // When a command goes to the `sendMessage` port, we pass the message
    // along to the WebSocket.
    // app.ports.sendMessage.subscribe(function (message) {
    //   socket.send(message);
    // });

    // When a message comes into our WebSocket, we pass the message along
    // to the `messageReceiver` port.
    socket.addEventListener("message", function (event) {
      if (event.data instanceof Blob) {
        const reader = new FileReader();

        reader.onload = () => {
          console.log("Result: " + reader.result);
        };

        reader.readAsText(event.data);
      } else {
        console.log("Result: " + event.data);
      }
      // console.log("Received message from server:", event.data);
      app.ports.messageReceiver.send(event.data);
    });
    socket.addEventListener("open", function (event) {
      console.log("Connected to server: ", event);
      // socket.send(
      //   `user test99123 pass -1 vers testsoftware 1.0_05 filter r/33.25/-96.5/50`
      // );
      // app.ports.socketConnected.send(event.data);
    });
    socket.addEventListener("error", function (event) {
      console.log("Error: ", event);
      // app.ports.socketError.send(event.data);
    });
    socket.addEventListener("close", function (event) {
      console.log("Closed: ", event);
      // app.ports.socketClosed.send(event);
    });
  });
})().catch(() =>
  displayErrorMessage("Something went wrong", "Please try again later")
);
