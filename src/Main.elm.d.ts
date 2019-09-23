export namespace Elm {
  namespace Main {
    interface App {
      ports: Ports;
    }
    interface Ports {
      imageDecoded: Send<{ width: number; height: number; data: number[] }>;
      decodeImage: Subscribe<string, void>;
      error: Send<string>;
    }
    interface Subscribe<T, U> {
      subscribe(callback: (value: T) => U): void;
    }
    interface Send<T> {
      send(value: T): void;
    }
    interface Port<T> {}
    interface Flags {
      repositoryUrl: string;
      title: string;
    }
    function init(value: { flags: Flags }): App;
  }
}

// vim: set ts=2 sw=2 et:
