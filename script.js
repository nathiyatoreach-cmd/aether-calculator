const DIVISION_PRECISION = 32;
const DISPLAY_MAX_CHARS = 22;
const INPUT_MAX_CHARS = 42;
const HISTORY_LIMIT = 14;
const FACTORIAL_LIMIT = 2000;

const POW10_CACHE = [1n];

function pow10(exponent) {
  const safeExponent = Math.max(0, exponent);

  while (POW10_CACHE.length <= safeExponent) {
    POW10_CACHE.push(POW10_CACHE[POW10_CACHE.length - 1] * 10n);
  }

  return POW10_CACHE[safeExponent];
}

function absBigInt(value) {
  return value < 0n ? -value : value;
}

function isDigit(char) {
  return char >= "0" && char <= "9";
}

function stripTrailingZeros(text) {
  if (!text.includes(".")) {
    return text.replace(/^\+/, "");
  }

  return text
    .replace(/(\.\d*?[1-9])0+$/, "$1")
    .replace(/\.0+$/, "")
    .replace(/\.$/, "")
    .replace(/^\+/, "");
}

function numberToPortableString(value, significantDigits = 15) {
  if (!Number.isFinite(value)) {
    throw new Error("Result overflowed the supported range.");
  }

  if (Object.is(value, -0)) {
    return "0";
  }

  const rounded = Math.abs(value) < 1e-15 ? 0 : value;
  const text = rounded.toPrecision(significantDigits);

  if (/e/i.test(text)) {
    const [mantissa, exponent] = text.toLowerCase().split("e");
    return `${stripTrailingZeros(mantissa)}e${Number(exponent)}`;
  }

  return stripTrailingZeros(text);
}

class Decimal {
  constructor(coefficient, scale = 0, approximate = false) {
    this.coefficient = BigInt(coefficient);
    this.scale = scale;
    this.approximate = approximate;
    this.normalize();
  }

  static zero() {
    return new Decimal(0n, 0, false);
  }

  static one() {
    return new Decimal(1n, 0, false);
  }

  static fromString(raw, approximate = false) {
    const text = String(raw).trim();
    const match = text.match(/^([+-]?)(?:(\d+)(?:\.(\d*))?|\.(\d+))(?:[eE]([+-]?\d+))?$/);

    if (!match) {
      throw new Error(`Invalid number "${raw}".`);
    }

    const sign = match[1] === "-" ? -1n : 1n;
    const integerPart = match[2] ?? "";
    const fractionPart = match[3] ?? match[4] ?? "";
    const exponent = Number(match[5] ?? 0);

    let digits = `${integerPart}${fractionPart}`.replace(/^0+(?=\d)/, "");
    let scale = fractionPart.length - exponent;

    if (!digits) {
      return new Decimal(0n, 0, approximate);
    }

    if (scale < 0) {
      digits += "0".repeat(-scale);
      scale = 0;
    }

    return new Decimal(sign * BigInt(digits), scale, approximate);
  }

  static fromNumber(value, approximate = true) {
    return Decimal.fromString(numberToPortableString(value), approximate);
  }

  clone() {
    return new Decimal(this.coefficient, this.scale, this.approximate);
  }

  normalize() {
    if (this.coefficient === 0n) {
      this.scale = 0;
      return this;
    }

    while (this.scale > 0 && this.coefficient % 10n === 0n) {
      this.coefficient /= 10n;
      this.scale -= 1;
    }

    return this;
  }

  isZero() {
    return this.coefficient === 0n;
  }

  isInteger() {
    return this.scale === 0;
  }

  negate() {
    return new Decimal(-this.coefficient, this.scale, this.approximate);
  }

  abs() {
    return this.coefficient < 0n ? this.negate() : this.clone();
  }

  compare(other) {
    const scale = Math.max(this.scale, other.scale);
    const left = this.coefficient * pow10(scale - this.scale);
    const right = other.coefficient * pow10(scale - other.scale);

    if (left < right) {
      return -1;
    }

    if (left > right) {
      return 1;
    }

    return 0;
  }

  add(other) {
    const scale = Math.max(this.scale, other.scale);
    const left = this.coefficient * pow10(scale - this.scale);
    const right = other.coefficient * pow10(scale - other.scale);

    return new Decimal(
      left + right,
      scale,
      this.approximate || other.approximate,
    );
  }

  subtract(other) {
    return this.add(other.negate());
  }

  multiply(other) {
    return new Decimal(
      this.coefficient * other.coefficient,
      this.scale + other.scale,
      this.approximate || other.approximate,
    );
  }

  divide(other, precision = DIVISION_PRECISION) {
    if (other.isZero()) {
      throw new Error("Cannot divide by zero.");
    }

    if (this.isZero()) {
      return new Decimal(0n, 0, this.approximate || other.approximate);
    }

    const negative = (this.coefficient < 0n) !== (other.coefficient < 0n);
    const numerator = absBigInt(this.coefficient) * pow10(precision + other.scale);
    const denominator = absBigInt(other.coefficient) * pow10(this.scale);
    let quotient = numerator / denominator;
    const remainder = numerator % denominator;

    if (remainder !== 0n) {
      const nextDigit = (remainder * 10n) / denominator;

      if (nextDigit >= 5n) {
        quotient += 1n;
      }
    }

    return new Decimal(
      negative ? -quotient : quotient,
      precision,
      this.approximate || other.approximate || remainder !== 0n,
    );
  }

  powInt(exponent, precision = DIVISION_PRECISION) {
    if (exponent === 0) {
      return Decimal.one();
    }

    if (exponent < 0) {
      return Decimal.one().divide(this.powInt(-exponent, precision), precision);
    }

    let result = Decimal.one();
    let base = this.clone();
    let power = exponent;

    while (power > 0) {
      if (power % 2 === 1) {
        result = result.multiply(base);
      }

      base = base.multiply(base);
      power = Math.floor(power / 2);
    }

    result.approximate = result.approximate || this.approximate;
    return result;
  }

  truncate() {
    if (this.scale === 0) {
      return this.clone();
    }

    const factor = pow10(this.scale);
    return new Decimal(this.coefficient / factor, 0, this.approximate);
  }

  floor() {
    if (this.scale === 0) {
      return this.clone();
    }

    const factor = pow10(this.scale);
    let integer = this.coefficient / factor;
    const remainder = this.coefficient % factor;

    if (this.coefficient < 0n && remainder !== 0n) {
      integer -= 1n;
    }

    return new Decimal(integer, 0, this.approximate);
  }

  ceil() {
    if (this.scale === 0) {
      return this.clone();
    }

    const factor = pow10(this.scale);
    let integer = this.coefficient / factor;
    const remainder = this.coefficient % factor;

    if (this.coefficient > 0n && remainder !== 0n) {
      integer += 1n;
    }

    return new Decimal(integer, 0, this.approximate);
  }

  round(decimalPlaces = 0) {
    if (!Number.isInteger(decimalPlaces)) {
      throw new Error("round() expects an integer number of decimal places.");
    }

    if (decimalPlaces >= this.scale) {
      return this.clone();
    }

    if (decimalPlaces >= 0) {
      const drop = this.scale - decimalPlaces;
      const factor = pow10(drop);
      let base = this.coefficient / factor;
      const remainder = absBigInt(this.coefficient % factor);

      if (remainder * 2n >= factor) {
        base += this.coefficient >= 0n ? 1n : -1n;
      }

      return new Decimal(base, decimalPlaces, this.approximate);
    }

    const shift = -decimalPlaces;
    const shifted = new Decimal(this.coefficient, this.scale + shift, this.approximate);
    const rounded = shifted.round(0);
    return new Decimal(rounded.coefficient, Math.max(0, rounded.scale - shift), this.approximate);
  }

  toPlainString() {
    if (this.coefficient === 0n) {
      return "0";
    }

    const negative = this.coefficient < 0n;
    const digits = absBigInt(this.coefficient).toString();

    if (this.scale === 0) {
      return `${negative ? "-" : ""}${digits}`;
    }

    if (this.scale >= digits.length) {
      const leading = "0".repeat(this.scale - digits.length);
      return `${negative ? "-" : ""}0.${leading}${digits}`;
    }

    const split = digits.length - this.scale;
    return `${negative ? "-" : ""}${digits.slice(0, split)}.${digits.slice(split)}`;
  }

  toScientificString(significantDigits = 14) {
    if (this.coefficient === 0n) {
      return "0";
    }

    const negative = this.coefficient < 0n ? "-" : "";
    const digits = absBigInt(this.coefficient).toString();
    const exponent = digits.length - this.scale - 1;
    const mantissaDigits = digits.slice(0, significantDigits);
    const head = mantissaDigits[0];
    const tail = mantissaDigits.slice(1);
    const mantissa = tail ? `${head}.${stripTrailingZeros(tail)}` : head;

    return `${negative}${mantissa}e${exponent}`;
  }

  toDisplayString(maxChars = DISPLAY_MAX_CHARS) {
    const plain = this.toPlainString();

    if (plain.length <= maxChars) {
      return plain;
    }

    return this.toScientificString(Math.max(10, maxChars - 6));
  }

  toInputString(maxChars = INPUT_MAX_CHARS) {
    const plain = this.toPlainString();

    if (plain.length <= maxChars) {
      return plain;
    }

    return this.toScientificString(Math.max(14, maxChars - 8));
  }

  toNumber() {
    return Number(this.toPlainString());
  }
}

function integerSqrt(value) {
  if (value < 0n) {
    throw new Error("Cannot take the square root of a negative value.");
  }

  if (value < 2n) {
    return value;
  }

  let current = value;
  let next = (current + value / current) >> 1n;

  while (next < current) {
    current = next;
    next = (current + value / current) >> 1n;
  }

  return current;
}

function normalizeExpression(raw) {
  return String(raw ?? "")
    .replace(/×/g, "*")
    .replace(/÷/g, "/")
    .replace(/−/g, "-")
    .replace(/π/gi, "pi")
    .replace(/τ/gi, "tau")
    .replace(/√/g, "sqrt")
    .trim();
}

function tokenize(expression) {
  const source = normalizeExpression(expression);
  const tokens = [];
  let index = 0;

  while (index < source.length) {
    const char = source[index];

    if (/\s/.test(char)) {
      index += 1;
      continue;
    }

    if (isDigit(char) || (char === "." && isDigit(source[index + 1] ?? ""))) {
      const start = index;
      let seenDot = false;

      while (index < source.length) {
        const current = source[index];

        if (current === ".") {
          if (seenDot) {
            break;
          }

          seenDot = true;
          index += 1;
          continue;
        }

        if (!isDigit(current)) {
          break;
        }

        index += 1;
      }

      if ((source[index] === "e" || source[index] === "E") && /[+\-\d]/.test(source[index + 1] ?? "")) {
        index += 1;

        if (source[index] === "+" || source[index] === "-") {
          index += 1;
        }

        const exponentStart = index;

        while (isDigit(source[index] ?? "")) {
          index += 1;
        }

        if (exponentStart === index) {
          throw new Error("Scientific notation is incomplete.");
        }
      }

      tokens.push({ type: "number", value: source.slice(start, index) });
      continue;
    }

    if (/[a-z_]/i.test(char)) {
      const start = index;

      while (/[a-z0-9_]/i.test(source[index] ?? "")) {
        index += 1;
      }

      tokens.push({ type: "identifier", value: source.slice(start, index).toLowerCase() });
      continue;
    }

    if (char === "(") {
      tokens.push({ type: "leftParen", value: char });
      index += 1;
      continue;
    }

    if (char === ")") {
      tokens.push({ type: "rightParen", value: char });
      index += 1;
      continue;
    }

    if (char === ",") {
      tokens.push({ type: "comma", value: char });
      index += 1;
      continue;
    }

    if ("+-*/^%!".includes(char)) {
      tokens.push({ type: "operator", value: char });
      index += 1;
      continue;
    }

    throw new Error(`Unsupported character "${char}".`);
  }

  return insertImplicitMultiplication(tokens);
}

function canEndValue(token) {
  return (
    token.type === "number" ||
    token.type === "identifier" ||
    token.type === "rightParen" ||
    (token.type === "operator" && token.value === "!")
  );
}

function canStartValue(token) {
  return token.type === "number" || token.type === "identifier" || token.type === "leftParen";
}

function insertImplicitMultiplication(tokens) {
  const output = [];

  for (const token of tokens) {
    const previous = output[output.length - 1];

    if (
      previous &&
      canEndValue(previous) &&
      canStartValue(token) &&
      !(previous.type === "identifier" && token.type === "leftParen")
    ) {
      output.push({ type: "operator", value: "*", implicit: true });
    }

    output.push(token);
  }

  return output;
}

const INFIX_PRECEDENCE = {
  "+": { precedence: 10, rightAssociative: false },
  "-": { precedence: 10, rightAssociative: false },
  "*": { precedence: 20, rightAssociative: false },
  "/": { precedence: 20, rightAssociative: false },
  "%": { precedence: 20, rightAssociative: false },
  "^": { precedence: 30, rightAssociative: true },
};

const PREFIX_PRECEDENCE = 25;
const POSTFIX_PRECEDENCE = 40;

class Parser {
  constructor(tokens) {
    this.tokens = tokens;
    this.index = 0;
  }

  peek() {
    return this.tokens[this.index];
  }

  next() {
    const token = this.tokens[this.index];
    this.index += 1;
    return token;
  }

  check(type, value) {
    const token = this.peek();

    if (!token || token.type !== type) {
      return false;
    }

    return value === undefined ? true : token.value === value;
  }

  match(type, value) {
    if (!this.check(type, value)) {
      return false;
    }

    this.index += 1;
    return true;
  }

  expect(type, value, message) {
    if (!this.check(type, value)) {
      throw new Error(message);
    }

    return this.next();
  }

  parse() {
    const expression = this.parseExpression(0);

    if (this.peek()) {
      throw new Error(`Unexpected token "${this.peek().value}".`);
    }

    return expression;
  }

  parseExpression(minPrecedence = 0) {
    let left = this.parsePrefix();

    while (true) {
      const token = this.peek();

      if (!token) {
        break;
      }

      if (token.type === "operator" && token.value === "!") {
        if (POSTFIX_PRECEDENCE < minPrecedence) {
          break;
        }

        this.next();
        left = { type: "postfix", operator: "!", operand: left };
        continue;
      }

      if (token.type !== "operator" || !INFIX_PRECEDENCE[token.value]) {
        break;
      }

      const { precedence, rightAssociative } = INFIX_PRECEDENCE[token.value];

      if (precedence < minPrecedence) {
        break;
      }

      this.next();
      const right = this.parseExpression(rightAssociative ? precedence : precedence + 1);
      left = { type: "binary", operator: token.value, left, right };
    }

    return left;
  }

  parsePrefix() {
    const token = this.next();

    if (!token) {
      throw new Error("Unexpected end of expression.");
    }

    if (token.type === "number") {
      return { type: "number", value: token.value };
    }

    if (token.type === "identifier") {
      if (this.match("leftParen")) {
        const args = [];

        if (!this.check("rightParen")) {
          do {
            args.push(this.parseExpression(0));
          } while (this.match("comma"));
        }

        this.expect("rightParen", undefined, `Missing closing parenthesis for ${token.value}().`);
        return { type: "call", name: token.value, args };
      }

      return { type: "identifier", name: token.value };
    }

    if (token.type === "leftParen") {
      const expression = this.parseExpression(0);
      this.expect("rightParen", undefined, "Missing closing parenthesis.");
      return expression;
    }

    if (token.type === "operator" && (token.value === "+" || token.value === "-")) {
      return {
        type: "unary",
        operator: token.value,
        operand: this.parseExpression(PREFIX_PRECEDENCE),
      };
    }

    throw new Error(`Unexpected token "${token.value}".`);
  }
}

function parseExpression(expression) {
  const tokens = tokenize(expression);

  if (tokens.length === 0) {
    throw new Error("Enter an expression to calculate.");
  }

  const parser = new Parser(tokens);
  return parser.parse();
}

function exactSqrt(value) {
  let coefficient = absBigInt(value.coefficient);
  let scale = value.scale;

  if (scale % 2 !== 0) {
    coefficient *= 10n;
    scale += 1;
  }

  const root = integerSqrt(coefficient);

  if (root * root !== coefficient) {
    return null;
  }

  return new Decimal(root, scale / 2, value.approximate);
}

function toFiniteNumber(value, message) {
  const numeric = value.toNumber();

  if (!Number.isFinite(numeric)) {
    throw new Error(message ?? "Value is outside the supported range.");
  }

  return numeric;
}

function approximateResult(value) {
  return Decimal.fromNumber(value, true);
}

function angleToRadians(value, angleMode) {
  const numeric = toFiniteNumber(value, "Angle is too large for trigonometry.");
  return angleMode === "DEG" ? (numeric * Math.PI) / 180 : numeric;
}

function radiansToAngle(value, angleMode) {
  return angleMode === "DEG" ? (value * 180) / Math.PI : value;
}

function assertInteger(value, label) {
  if (!value.isInteger()) {
    throw new Error(`${label} must be an integer.`);
  }
}

function assertNonNegative(value, label) {
  if (value.compare(Decimal.zero()) < 0) {
    throw new Error(`${label} must be non-negative.`);
  }
}

function factorial(value) {
  assertInteger(value, "Factorial input");
  assertNonNegative(value, "Factorial input");

  const numeric = Number(value.toPlainString());

  if (!Number.isFinite(numeric) || numeric > FACTORIAL_LIMIT) {
    throw new Error(`Factorial is limited to ${FACTORIAL_LIMIT} for speed.`);
  }

  let result = 1n;

  for (let current = 2n; current <= BigInt(numeric); current += 1n) {
    result *= current;
  }

  return new Decimal(result, 0, value.approximate);
}

function power(left, right) {
  if (right.isInteger()) {
    const exponent = Number(right.toPlainString());

    if (Math.abs(exponent) > 5000) {
      throw new Error("Integer powers are limited to +/-5000.");
    }

    return left.powInt(exponent);
  }

  const base = toFiniteNumber(left, "Base is too large for fractional powers.");
  const exponent = toFiniteNumber(right, "Exponent is too large for fractional powers.");
  return approximateResult(Math.pow(base, exponent));
}

function modulo(left, right) {
  assertInteger(left, "Modulo left side");
  assertInteger(right, "Modulo right side");

  if (right.isZero()) {
    throw new Error("Cannot take modulo by zero.");
  }

  return new Decimal(left.coefficient % right.coefficient, 0, left.approximate || right.approximate);
}

function callFunction(name, args, context) {
  const angleMode = context.angleMode ?? "RAD";

  switch (name) {
    case "sqrt": {
      if (args.length !== 1) {
        throw new Error("sqrt() expects 1 argument.");
      }

      assertNonNegative(args[0], "sqrt()");
      const exact = exactSqrt(args[0]);
      return exact ?? approximateResult(Math.sqrt(toFiniteNumber(args[0], "sqrt() input is too large.")));
    }

    case "cbrt": {
      if (args.length !== 1) {
        throw new Error("cbrt() expects 1 argument.");
      }

      return approximateResult(Math.cbrt(toFiniteNumber(args[0], "cbrt() input is too large.")));
    }

    case "sin": {
      if (args.length !== 1) {
        throw new Error("sin() expects 1 argument.");
      }

      return approximateResult(Math.sin(angleToRadians(args[0], angleMode)));
    }

    case "cos": {
      if (args.length !== 1) {
        throw new Error("cos() expects 1 argument.");
      }

      return approximateResult(Math.cos(angleToRadians(args[0], angleMode)));
    }

    case "tan": {
      if (args.length !== 1) {
        throw new Error("tan() expects 1 argument.");
      }

      return approximateResult(Math.tan(angleToRadians(args[0], angleMode)));
    }

    case "asin": {
      if (args.length !== 1) {
        throw new Error("asin() expects 1 argument.");
      }

      const input = toFiniteNumber(args[0], "asin() input is too large.");

      if (input < -1 || input > 1) {
        throw new Error("asin() input must be between -1 and 1.");
      }

      return approximateResult(radiansToAngle(Math.asin(input), angleMode));
    }

    case "acos": {
      if (args.length !== 1) {
        throw new Error("acos() expects 1 argument.");
      }

      const input = toFiniteNumber(args[0], "acos() input is too large.");

      if (input < -1 || input > 1) {
        throw new Error("acos() input must be between -1 and 1.");
      }

      return approximateResult(radiansToAngle(Math.acos(input), angleMode));
    }

    case "atan": {
      if (args.length !== 1) {
        throw new Error("atan() expects 1 argument.");
      }

      return approximateResult(radiansToAngle(Math.atan(toFiniteNumber(args[0], "atan() input is too large.")), angleMode));
    }

    case "ln": {
      if (args.length !== 1) {
        throw new Error("ln() expects 1 argument.");
      }

      const input = toFiniteNumber(args[0], "ln() input is too large.");

      if (input <= 0) {
        throw new Error("ln() input must be greater than zero.");
      }

      return approximateResult(Math.log(input));
    }

    case "log": {
      if (args.length === 1) {
        const input = toFiniteNumber(args[0], "log() input is too large.");

        if (input <= 0) {
          throw new Error("log() input must be greater than zero.");
        }

        return approximateResult(Math.log10(input));
      }

      if (args.length === 2) {
        const base = toFiniteNumber(args[0], "log() base is too large.");
        const input = toFiniteNumber(args[1], "log() input is too large.");

        if (base <= 0 || base === 1 || input <= 0) {
          throw new Error("log(base, value) needs base > 0, base != 1, and value > 0.");
        }

        return approximateResult(Math.log(input) / Math.log(base));
      }

      throw new Error("log() expects 1 or 2 arguments.");
    }

    case "exp": {
      if (args.length !== 1) {
        throw new Error("exp() expects 1 argument.");
      }

      return approximateResult(Math.exp(toFiniteNumber(args[0], "exp() input is too large.")));
    }

    case "abs": {
      if (args.length !== 1) {
        throw new Error("abs() expects 1 argument.");
      }

      return args[0].abs();
    }

    case "floor": {
      if (args.length !== 1) {
        throw new Error("floor() expects 1 argument.");
      }

      return args[0].floor();
    }

    case "ceil": {
      if (args.length !== 1) {
        throw new Error("ceil() expects 1 argument.");
      }

      return args[0].ceil();
    }

    case "round": {
      if (args.length === 1) {
        return args[0].round(0);
      }

      if (args.length === 2) {
        assertInteger(args[1], "round() precision");
        return args[0].round(Number(args[1].toPlainString()));
      }

      throw new Error("round() expects 1 or 2 arguments.");
    }

    case "pow": {
      if (args.length !== 2) {
        throw new Error("pow() expects 2 arguments.");
      }

      return power(args[0], args[1]);
    }

    case "root": {
      if (args.length !== 2) {
        throw new Error("root() expects 2 arguments.");
      }

      const degree = toFiniteNumber(args[1], "root() degree is too large.");
      const input = toFiniteNumber(args[0], "root() input is too large.");
      return approximateResult(Math.pow(input, 1 / degree));
    }

    case "min": {
      if (args.length < 1) {
        throw new Error("min() expects at least 1 argument.");
      }

      return args.reduce((smallest, current) => (current.compare(smallest) < 0 ? current : smallest));
    }

    case "max": {
      if (args.length < 1) {
        throw new Error("max() expects at least 1 argument.");
      }

      return args.reduce((largest, current) => (current.compare(largest) > 0 ? current : largest));
    }

    default:
      throw new Error(`Unknown function "${name}()".`);
  }
}

function resolveIdentifier(name, context) {
  switch (name) {
    case "pi":
      return Decimal.fromNumber(Math.PI, true);
    case "e":
      return Decimal.fromNumber(Math.E, true);
    case "tau":
      return Decimal.fromNumber(Math.PI * 2, true);
    case "phi":
      return Decimal.fromNumber((1 + Math.sqrt(5)) / 2, true);
    case "ans":
      return (context.answer ?? Decimal.zero()).clone();
    case "mem":
      return (context.memory ?? Decimal.zero()).clone();
    default:
      throw new Error(`Unknown constant "${name}".`);
  }
}

function evaluateAst(node, context) {
  switch (node.type) {
    case "number":
      return Decimal.fromString(node.value);

    case "identifier":
      return resolveIdentifier(node.name, context);

    case "unary": {
      const value = evaluateAst(node.operand, context);
      return node.operator === "-" ? value.negate() : value;
    }

    case "postfix": {
      const value = evaluateAst(node.operand, context);
      return factorial(value);
    }

    case "binary": {
      const left = evaluateAst(node.left, context);
      const right = evaluateAst(node.right, context);

      switch (node.operator) {
        case "+":
          return left.add(right);
        case "-":
          return left.subtract(right);
        case "*":
          return left.multiply(right);
        case "/":
          return left.divide(right);
        case "%":
          return modulo(left, right);
        case "^":
          return power(left, right);
        default:
          throw new Error(`Unknown operator "${node.operator}".`);
      }
    }

    case "call":
      return callFunction(
        node.name,
        node.args.map((child) => evaluateAst(child, context)),
        context,
      );

    default:
      throw new Error("Unsupported expression tree.");
  }
}

function evaluateExpression(expression, context = {}) {
  const ast = parseExpression(expression);
  return evaluateAst(ast, context);
}

function makeDefaultState() {
  return {
    angleMode: "RAD",
    answer: Decimal.zero(),
    memory: null,
    preview: null,
    history: [],
  };
}

function isIncompleteExpression(message) {
  return (
    message.includes("Unexpected end of expression") ||
    message.includes("Missing closing parenthesis") ||
    message.includes("Scientific notation is incomplete")
  );
}

function buildUi() {
  const elements = {
    input: document.getElementById("expression-input"),
    resultOutput: document.getElementById("result-output"),
    resultBadge: document.getElementById("result-badge"),
    statusLine: document.getElementById("status-line"),
    memoryLine: document.getElementById("memory-line"),
    historyList: document.getElementById("history-list"),
    keypad: document.getElementById("keypad"),
    angleToggle: document.getElementById("angle-toggle"),
    copyResult: document.getElementById("copy-result"),
  };

  const state = makeDefaultState();
  let previewFrame = 0;

  function context() {
    return {
      angleMode: state.angleMode,
      answer: state.answer,
      memory: state.memory ?? Decimal.zero(),
    };
  }

  function updateBadge(label, tone = "default") {
    elements.resultBadge.textContent = label;
    elements.resultBadge.classList.remove("is-exact", "is-error");

    if (tone === "exact") {
      elements.resultBadge.classList.add("is-exact");
    }

    if (tone === "error") {
      elements.resultBadge.classList.add("is-error");
    }
  }

  function updateMemoryLine() {
    if (!state.memory) {
      elements.memoryLine.textContent = "Memory: empty";
      return;
    }

    elements.memoryLine.textContent = `Memory: ${state.memory.toDisplayString(24)}`;
  }

  function setResult(result, statusMessage) {
    state.preview = result;
    elements.resultOutput.textContent = result.toDisplayString();

    if (result.approximate) {
      updateBadge("APPROX", "default");
    } else {
      updateBadge("EXACT", "exact");
    }

    elements.statusLine.textContent = statusMessage;
  }

  function setReady() {
    state.preview = null;
    elements.resultOutput.textContent = state.answer.toDisplayString();
    updateBadge("READY", "default");
    elements.statusLine.textContent = "Exact decimal engine ready for +, -, *, /, and integer powers.";
  }

  function setError(message, keepOutput = true) {
    if (!keepOutput) {
      elements.resultOutput.textContent = "...";
    }

    updateBadge("ERROR", "error");
    elements.statusLine.textContent = message;
  }

  function renderHistory() {
    elements.historyList.replaceChildren();

    if (state.history.length === 0) {
      const empty = document.createElement("div");
      empty.className = "history-empty";
      empty.textContent = "No calculations yet.";
      elements.historyList.append(empty);
      return;
    }

    for (const entry of state.history) {
      const item = document.createElement("button");
      item.type = "button";
      item.className = "history-item";
      item.dataset.result = entry.resultInput;

      const expression = document.createElement("span");
      expression.className = "history-expression";
      expression.textContent = entry.expression;

      const result = document.createElement("span");
      result.className = "history-result";
      result.textContent = entry.resultDisplay;

      const tag = document.createElement("span");
      tag.className = "history-tag";
      tag.textContent = entry.approximate ? "Approx result" : "Exact result";

      item.append(expression, result, tag);
      elements.historyList.append(item);
    }
  }

  function pushHistory(expression, result) {
    state.history.unshift({
      expression,
      resultInput: result.toInputString(),
      resultDisplay: result.toDisplayString(28),
      approximate: result.approximate,
    });
    state.history = state.history.slice(0, HISTORY_LIMIT);
    renderHistory();
  }

  function replaceSelection(text) {
    const input = elements.input;
    const start = input.selectionStart ?? input.value.length;
    const end = input.selectionEnd ?? input.value.length;
    const nextValue = `${input.value.slice(0, start)}${text}${input.value.slice(end)}`;
    const caret = start + text.length;
    input.value = nextValue;
    input.focus();
    input.setSelectionRange(caret, caret);
    schedulePreview();
  }

  function wrapSelection(prefix, suffix = ")") {
    const input = elements.input;
    const start = input.selectionStart ?? input.value.length;
    const end = input.selectionEnd ?? input.value.length;
    const selected = input.value.slice(start, end);
    const wrapped = `${prefix}${selected}${suffix}`;
    const nextValue = `${input.value.slice(0, start)}${wrapped}${input.value.slice(end)}`;
    const caret = selected ? start + wrapped.length : start + prefix.length;

    input.value = nextValue;
    input.focus();
    input.setSelectionRange(caret, caret);
    schedulePreview();
  }

  function clearEntry() {
    elements.input.value = "";
    setReady();
    elements.input.focus();
  }

  function allClear() {
    state.answer = Decimal.zero();
    state.preview = null;
    elements.input.value = "";
    setReady();
    updateMemoryLine();
    elements.input.focus();
  }

  function backspace() {
    const input = elements.input;
    const start = input.selectionStart ?? input.value.length;
    const end = input.selectionEnd ?? input.value.length;

    if (start !== end) {
      input.value = `${input.value.slice(0, start)}${input.value.slice(end)}`;
      input.setSelectionRange(start, start);
      schedulePreview();
      return;
    }

    if (start === 0) {
      return;
    }

    input.value = `${input.value.slice(0, start - 1)}${input.value.slice(end)}`;
    input.setSelectionRange(start - 1, start - 1);
    schedulePreview();
  }

  function getLiveValue() {
    if (state.preview) {
      return state.preview.clone();
    }

    const expression = elements.input.value.trim();

    if (!expression) {
      return state.answer.clone();
    }

    return evaluateExpression(expression, context());
  }

  function adjustMemory(direction) {
    try {
      const value = getLiveValue();
      const current = state.memory ? state.memory.clone() : Decimal.zero();
      state.memory = direction === "add" ? current.add(value) : current.subtract(value);
      updateMemoryLine();
      elements.statusLine.textContent = direction === "add" ? "Current result added to memory." : "Current result subtracted from memory.";
    } catch (error) {
      setError(error.message);
    }
  }

  function recallMemory() {
    if (!state.memory) {
      setError("Memory is empty.");
      return;
    }

    replaceSelection(state.memory.toInputString());
  }

  function copyResult() {
    const source = state.preview ?? state.answer;
    const payload = source.toPlainString();

    if (!navigator.clipboard?.writeText) {
      setError("Clipboard access is not available in this browser.");
      return;
    }

    navigator.clipboard.writeText(payload)
      .then(() => {
        elements.statusLine.textContent = "Result copied to clipboard.";
      })
      .catch(() => {
        setError("Could not copy the result.");
      });
  }

  function commitEvaluation() {
    const expression = elements.input.value.trim();

    if (!expression) {
      setError("Enter an expression to calculate.");
      return;
    }

    try {
      const result = evaluateExpression(expression, context());
      state.answer = result.clone();
      setResult(result, result.approximate ? "Approximate result stored in ANS and history." : "Exact result stored in ANS and history.");
      pushHistory(expression, result);
      elements.input.value = result.toInputString();
      elements.input.setSelectionRange(elements.input.value.length, elements.input.value.length);
    } catch (error) {
      setError(error.message, false);
    }
  }

  function schedulePreview() {
    if (previewFrame) {
      cancelAnimationFrame(previewFrame);
    }

    previewFrame = requestAnimationFrame(() => {
      previewFrame = 0;
      refreshPreview();
    });
  }

  function refreshPreview() {
    const expression = elements.input.value.trim();

    if (!expression) {
      setReady();
      return;
    }

    try {
      const result = evaluateExpression(expression, context());
      setResult(
        result,
        result.approximate
          ? "Live preview is approximate because the expression uses advanced functions or repeating decimals."
          : "Live preview is exact.",
      );
    } catch (error) {
      state.preview = null;

      if (isIncompleteExpression(error.message)) {
        updateBadge("READY", "default");
        elements.resultOutput.textContent = state.answer.toDisplayString();
        elements.statusLine.textContent = "Keep typing to complete the expression.";
        return;
      }

      setError(error.message);
    }
  }

  function setAngleMode(mode) {
    state.angleMode = mode;
    elements.angleToggle.textContent = mode;
    elements.angleToggle.setAttribute("aria-pressed", String(mode === "DEG"));
    elements.statusLine.textContent = mode === "DEG" ? "Degree mode enabled." : "Radian mode enabled.";
    schedulePreview();
  }

  function toggleAngleMode() {
    setAngleMode(state.angleMode === "RAD" ? "DEG" : "RAD");
  }

  function clearHistory() {
    state.history = [];
    renderHistory();
    elements.statusLine.textContent = "History cleared.";
  }

  function handleAction(action) {
    switch (action) {
      case "all-clear":
        allClear();
        return;
      case "clear-entry":
        clearEntry();
        return;
      case "backspace":
        backspace();
        return;
      case "evaluate":
        commitEvaluation();
        return;
      case "memory-clear":
        state.memory = null;
        updateMemoryLine();
        elements.statusLine.textContent = "Memory cleared.";
        return;
      case "memory-recall":
        recallMemory();
        return;
      case "memory-add":
        adjustMemory("add");
        return;
      case "memory-subtract":
        adjustMemory("subtract");
        return;
      case "use-answer":
        replaceSelection("ans");
        return;
      case "clear-history":
        clearHistory();
        return;
      case "square":
        replaceSelection("^2");
        return;
      case "negate-next":
        if ((elements.input.selectionStart ?? 0) !== (elements.input.selectionEnd ?? 0)) {
          wrapSelection("(-", ")");
        } else {
          replaceSelection("-");
        }
        return;
      default:
        break;
    }
  }

  elements.input.addEventListener("input", schedulePreview);

  elements.input.addEventListener("keydown", (event) => {
    if (event.key === "Enter") {
      event.preventDefault();
      commitEvaluation();
      return;
    }

    if (event.key === "Escape") {
      event.preventDefault();
      clearEntry();
      return;
    }

    if ((event.ctrlKey || event.metaKey) && event.key.toLowerCase() === "l") {
      event.preventDefault();
      allClear();
    }
  });

  elements.keypad.addEventListener("click", (event) => {
    const button = event.target.closest("button");

    if (!button) {
      return;
    }

    const { insert, action } = button.dataset;

    if (insert) {
      replaceSelection(insert);
      return;
    }

    if (action) {
      handleAction(action);
    }
  });

  elements.historyList.addEventListener("click", (event) => {
    const item = event.target.closest(".history-item");

    if (!item) {
      return;
    }

    elements.input.value = item.dataset.result ?? "";
    elements.input.focus();
    elements.input.setSelectionRange(elements.input.value.length, elements.input.value.length);
    schedulePreview();
  });

  elements.angleToggle.addEventListener("click", toggleAngleMode);
  elements.copyResult.addEventListener("click", copyResult);

  updateMemoryLine();
  renderHistory();
  setAngleMode("RAD");
  setReady();
}

const exportedApi = {
  Decimal,
  evaluateExpression,
  parseExpression,
  tokenize,
  normalizeExpression,
};

if (typeof window !== "undefined") {
  window.AetherCalculator = exportedApi;

  if (typeof document !== "undefined") {
    document.addEventListener("DOMContentLoaded", buildUi);
  }
}

if (typeof module !== "undefined" && module.exports) {
  module.exports = exportedApi;
}
