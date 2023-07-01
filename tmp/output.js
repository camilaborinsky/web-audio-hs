const audioContext = new AudioContext();
const oscillator1 = new OscillatorNode(audioContext, {"detune":0.1,"frequency":0.5,"type":"sine"});
const oscillator2 = new OscillatorNode(audioContext, {"detune":0.1,"frequency":0.5,"type":"sine"});
oscillator1.connect(oscillator2);
