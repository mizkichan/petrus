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
    interface Flags {
      repositoryUrl: string;
      title: string;
    }
    function init(value: { flags: Flags }): Elm;
  }
}

// vim: set ts=2 sw=2 et:
