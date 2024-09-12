import { Readable } from "stream";
import * as HTTP from "http";

class MockResponse extends HTTP.OutgoingMessage {
  body = "";

  /** @type {Record<string, string[]>} */
  headers = {};

  constructor() {
    super();
  }

  _implicitHeader() {}

  /** @type {(name: string, value: number | string | ReadonlyArray<string>) => this} */
  setHeader(k, v) {
    const vs =
      typeof v === "string" ? [v] : typeof v === "number" ? [v.toString()] : v;
    this.headers[k] = (this.headers[k] || []).concat(vs);
    return this;
  }

  /** @type {(k: string) => string[]} */
  getHeader(k) {
    return this.headers[k];
  }

  getHeaderNames() {
    return Object.keys(this.headers);
  }

  getHeaders() {
    return this.headers;
  }

  /** @type {(ck: any, encOrCb?: any, cb?: any) => boolean} */
  write(c, enc = null, cb_ = null) {
    this.body += c;
    const cb = cb_ || enc || (() => {});
    cb();
    return true;
  }

  /** @type {(ck: any, encOrCb?: any, cb?: any) => this} */
  end(ck = null, enc = null, cb_ = null) {
    let cb = cb_ || enc || ck || (() => {});
    cb();
    return this;
  }
}

/** @type {(a: string) => (b: string) => (c: string) => (d: string) => (e: Record<string, string>) => () => HTTP.IncomingMessage} */
export const mockRequestImpl =
  (httpVersion) => (method) => (url) => (body) => (headers) => () => {
    /** @type {any} */
    const stream = new Readable({
      read: function () {
        this.push(body);
        this.push(null);
      },
    });
    Object.assign(stream, { method, url, headers, httpVersion });

    return stream;
  };

/** @type {() => MockResponse} */
export const mockResponse = () => new MockResponse();

/** @type {(s: string) => Readable} */
export const stringToStream = (str) => {
  const stream = new Readable();
  stream._read = function () {};
  stream.push(str);
  stream.push(null);
  return stream;
};
