const audioContext = new AudioContext();
const oscillator1 = new OscillatorNode(audioContext, {"detune":1.5,"frequency":0.5,"type":"sine"});
const oscillator2 = new OscillatorNode(audioContext, {"detune":0.1,"frequency":0.5,"type":"sine"});
oscillator1.frequency.value = 440.0;
const detune = oscillator1.detune;
oscillator2.detune.value = 1.5;
const frequency = oscillator1.frequency;
oscillator2.connect(frequency);
oscillator1.connect(oscillator2);
