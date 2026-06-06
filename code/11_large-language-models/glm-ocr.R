# testing the glm-ocr model from ollama
# this is a language model specifically fine-tuned
# for OCR, so it may perform better than a
# larger, general-purpose LLM

library(rollama)
ping_ollama()
# pull_model('glm-ocr')

## Titanic article transcription (ellmer-tutorial.R) -------

titanic_transcription <-
  query('Transcribe this article.',
        model = 'glm-ocr',
        images = 'data/img/titanic.png',
        output = 'text')

cat(titanic_transcription)
# So much better!



