# prompting an open-source LLaMa model
# with the quallmer workflow. See setup instructions here:
# https://quallmer.github.io/quallmer/articles/pkgdown/getting-started/ollama.html

library(rollama)
# if you've set up ollama correctly, this code should run:
ping_ollama()

# this downloads the very smallest llama 3.2 (about 1.32 GB)
# pull_model("llama3.2:1b")

# if the download was successful, we can perform basic prompts with query()
query("What is the capital of Australia.", model = "llama3.2:1b")
# it's alive!

library(quallmer)

docs <- c('dog', 'cat', 'cinder block', 'dog house',
          'the color purple', 'sliced watermelon',
          'watermelon on the vine')

cb <- qlm_codebook(
  name = 'alive?',
  instructions = 'Is this thing alive?',
  schema = type_object(
    alive = type_boolean()
  )
)

coded <- qlm_code(docs, cb, model = 'ollama/llama3.2:1b',
                  name = 'run1')
coded$doc <- docs

# replicate
coded2 <- qlm_replicate(coded, name = 'run2')


qlm_compare(coded, coded2, by = 'alive')
# now we can see how it performs with more complex instructions
# in the quallmer tutorial


