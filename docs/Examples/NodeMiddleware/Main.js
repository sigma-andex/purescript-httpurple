export const logger = function (req, res, next) {
  console.log("Got a request");
  next();
};

export const authenticator = function (req, res, next) {
  if (req.headers["x-token"] == "123") {
    req.user = "John Doe";
  } else {
    req.user = null;
  }
  next();
};
