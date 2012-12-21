#include <cmath>
#include <iostream>
#include <sstream>
#include <vector>
#include <windows.h>
#include "SDL.h"
#include "SDL_mouse.h"
#include "SDL_main.h"

/* linker options: -lmingw32 -lSDLmain -lSDL -mwindows */

using namespace std;

double sampleFrequency = 0;
unsigned int audioBufferSize = 0;
unsigned int outputAudioBufferSize = 0;

const int num_waves = 2;
vector<double> freqs;
vector<double> amps;
vector<double> phases;


int mouse_x,mouse_y;

const double PI2 = 2*3.141592653;

void example_mixaudio(void *unused, Uint8 *stream, int len) {
  cerr << "called" << endl;

  for (int t=0;t<len;t++) {
    double sum = 0;
    for (int i = 0; i < num_waves; ++i) {
      sum += amps[i]*sin(phases[i]);
      phases[i] += freqs[i];
      while(phases[i]> 2*PI2)phases[i] -= 2*PI2;
    }
    int outputValue = int(sum);
    if (outputValue > 127) outputValue = 127;        // and clip the result
    if (outputValue < -128) outputValue = -128;      // this seems a crude method, but works very well

    stream[t] = int(outputValue);
  }

}

int main(int argc, char *argv[])
{

  if( SDL_Init(SDL_INIT_TIMER | SDL_INIT_AUDIO ) <0 ) {
    cout << "Unable to init SDL: " << SDL_GetError() << endl;
    return 1;
  }
  /* setup audio */
  SDL_AudioSpec *desired, *obtained;
  freqs.resize(num_waves);
  amps.resize(num_waves);
  phases.resize(num_waves);


  /* Allocate a desired SDL_AudioSpec */
  desired = (SDL_AudioSpec *) malloc(sizeof(SDL_AudioSpec));

  /* Allocate space for the obtained SDL_AudioSpec */
  obtained = (SDL_AudioSpec *) malloc(sizeof(SDL_AudioSpec));

  /* choose a samplerate and audio-format */
  desired->freq = 44100;
  desired->format = AUDIO_S8;

  /* Large audio buffers reduces risk of dropouts but increases response time.
   *
   * You should always check if you actually GOT the audiobuffer size you wanted,
   * note that not hardware supports all buffer sizes (< 2048 bytes gives problems with some
   * hardware). Older versions of SDL had a bug that caused many configuration to use a
   * buffersize of 11025 bytes, if your sdl.dll is approx. 1 Mb in stead of 220 Kb, download
   * v1.2.8 of SDL or better...)
   */
  desired->samples = 4096;

  /* Our callback function */
  desired->callback=example_mixaudio;
  desired->userdata=NULL;

  desired->channels = 1;



  /* Open the audio device and start playing sound! */
  if ( SDL_OpenAudio(desired, obtained) < 0 ) {
    fprintf(stderr, "AudioMixer, Unable to open audio: %s\n", SDL_GetError());
    exit(1);
  }

  audioBufferSize = obtained->samples;
  sampleFrequency = obtained->freq;



  /* if the format is 16 bit, two bytes are written for every sample */
  if (obtained->format==AUDIO_U16 || obtained->format==AUDIO_S16) {
    outputAudioBufferSize = 2*audioBufferSize;
  } else {
    outputAudioBufferSize = audioBufferSize;
  }

  SDL_Surface *screen = SDL_SetVideoMode(200,200, 16, SDL_SWSURFACE);
  SDL_WM_SetCaption("Audio Example",0);

  SDL_PauseAudio(0);

  bool running = true;

  // initiate wave banks
  for (int i = 0; i < num_waves; ++i) {
    double f = 800 + 0.3*double(i);
    freqs[i] = (6.28 * f / sampleFrequency);
    amps[i]  = 60;
    phases[i] = 3.14*i;
  }


  SDL_Event event;
  while (running) {
    while (SDL_PollEvent(&event)) {
      /* GLOBAL KEYS / EVENTS */
      switch (event.type) {
      case SDL_KEYDOWN:
	switch (event.key.keysym.sym) {
	case SDLK_ESCAPE:
	  running = false;
	  break;
	default: break;
	}
	break;
      case SDL_QUIT:
	running = false;
	break;
      }
      SDL_Delay(1);
      {
	SDL_GetMouseState(&mouse_x, &mouse_y);
	int k = GetAsyncKeyState(65);
	ostringstream ostr;
	ostr << mouse_x << " " << mouse_y  << " " << k;
	SDL_WM_SetCaption(ostr.str().c_str(),0);	
	  
      }
    }
    SDL_Delay(1);
  }
  SDL_Quit();
  return EXIT_SUCCESS;
}
