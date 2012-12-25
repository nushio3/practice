#include <iostream>
#include <cmath>
#include "SDL.h"
#include "SDL_main.h"

/* linker options: -lmingw32 -lSDLmain -lSDL -mwindows */

using namespace std;

unsigned int sampleFrequency = 0;
unsigned int audioBufferSize = 0;
unsigned int outputAudioBufferSize = 0;
Uint8 *hum_buf8;
Uint32 hum_len;
SDL_AudioSpec hum_spec;
int hum_ptr;

double freq1 = 800;
double fase1 = 0;


void example_mixaudio(void *unused, Uint8 *stream8, int len) {

  double bytesPerPeriod1 = sampleFrequency / freq1;
  Uint16 *stream = (Uint16*)(stream8);
  int16_t *hum_buf16 = (int16_t*)(hum_buf8);
  
  for (int i=0;i<len/2;i++) {
    int channel1 = 0*int(12050*sin(fase1*6.28/bytesPerPeriod1));
    int channel2 = int(hum_buf8[2*hum_ptr])*256+hum_buf8[2*hum_ptr+1];
    int outputValue = channel1 + hum_buf16[hum_ptr];//
    //+channel2/4;

    // just add the channels
    if (outputValue > 32767) outputValue = 32767;        // and clip the result
    if (outputValue < -32768) outputValue = -32768;      // this seems a crude method, but works very well

    stream[i] = outputValue;

    fase1++;
    //fase1 %= bytesPerPeriod1;
    hum_ptr ++;
    hum_ptr %= hum_len/2;
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

  /* Allocate a desired SDL_AudioSpec */
  desired = (SDL_AudioSpec *) malloc(sizeof(SDL_AudioSpec));

  /* Allocate space for the obtained SDL_AudioSpec */
  obtained = (SDL_AudioSpec *) malloc(sizeof(SDL_AudioSpec));

  /* choose a samplerate and audio-format */
  desired->freq = 44100;
  desired->format = AUDIO_S16;

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

  if(SDL_LoadWAV("HumLoop.wav", &hum_spec, &hum_buf8, &hum_len)==NULL){
    fprintf(stderr, "AudioMixer, Unable to load wav: %s\n", SDL_GetError());
    exit(1);
  }


  SDL_Surface *screen = SDL_SetVideoMode(200,200, 16, SDL_SWSURFACE);
  SDL_WM_SetCaption("Audio Example",0);
  
  SDL_PauseAudio(0);

  bool running = true;

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
    }
    SDL_Delay(1);
  }
  SDL_Quit();
  return EXIT_SUCCESS;
}