// @ts-ignore
import { Elm } from "./.elm-land/src/Main.elm";
import { displayErrorMessage } from "./errors";
// // @ts-ignore
// if (process.env.NODE_ENV === "development") {
//   const ElmDebugTransform = await import("elm-debug-transformer");

//   ElmDebugTransform.register({
//     simple_mode: true,
//   });
// }

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
      onboardingCompleted:
        localStorage.getItem("onboardingCompleted") === "true",
    },
    node: rootNode,
  });

  function subscribeToFlights() {
    const socket = new WebSocket(`ws://${location.hostname}:8081/watchFlights`);

    socket.addEventListener("message", (event) => {
      if (event.data instanceof Blob) {
        const reader = new FileReader();

        reader.readAsText(event.data);
      }

      app.ports.flightPositionReceiver.send(event.data);
    });

    socket.addEventListener("open", (event) => {
      console.log("Connected to server: ", event);
    });

    socket.addEventListener("error", (event) => {
      console.log("Error: ", event);
    });

    socket.addEventListener("close", (event) => {
      console.log("Closed: ", event);
      subscribeToFlights();
    });
  }

  app.ports.watchFlight.subscribe(subscribeToFlights);

  app.ports.onboardingCompleted.subscribe(() => {
    localStorage.setItem("onboardingCompleted", "true");
  });
})().catch(() =>
  displayErrorMessage("Something went wrong", "Please try again later")
);
