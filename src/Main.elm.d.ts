export namespace Elm {
  namespace Main {
    interface Elm {
      ports: Ports;
    }
    interface Ports {
      imageDecoded: Send<{ width: number; height: number; data: number[] }>;
      decodeImage: Subscribe<string>;
      error: Send<string>;
    }
    interface Subscribe<T> {
      subscribe(callback: (value: T) => void): void;
    }
    interface Send<T> {
      send(value: T): void;
    }
    function init(): Elm;
  }
}

// vim: set ts=2 sw=2 et:
