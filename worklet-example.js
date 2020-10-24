// lf-noise.js
class LFNoiseProcessor extends AudioWorkletProcessor {
  constructor() {
    super();
    this.nextIn = 0;
  }
  static get parameterDescriptors() {
    return [
      {
        name: "gain",
        defaultValue: 1,
        minValue: 0,
        maxValue: 1,
        automationRate: "a-rate",
      },
      {
        name: "freq",
        defaultValue: 440,
        minValue: 0,
        maxValue: 44100,
        automationRate: "a-rate",
      },
    ];
  }

  process(inputs, outputs, parameters) {
    const output = outputs[0];
    for (var j = 0; j < output.length; j++) {
      var channel = output[j];
      var origNextIn = this.nextIn;
      for (let i = 0; i < channel.length; i++) {
        var samplesBetween =
          sampleRate /
          (parameters["freq"].length > 1
            ? parameters["freq"][i]
            : parameters["freq"][0]);
        channel[i] =
          this.nextIn >= samplesBetween
            ? (Math.random() * 2 - 1) *
              (parameters["gain"].length > 1
                ? parameters["gain"][i]
                : parameters["gain"][0])
            : 0.0;

        this.nextIn = this.nextIn >= samplesBetween ? 0 : this.nextIn + 1;
      }
      if (j !== output.length - 1) {
        this.nextIn = origNextIn;
      }
    }
    return true;
  }
}

registerProcessor("klank-lf-noise", LFNoiseProcessor);
