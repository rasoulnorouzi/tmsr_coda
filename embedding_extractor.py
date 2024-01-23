import numpy as np
import pandas as pd
import stanza
from transformers import AutoTokenizer, AutoModel
import torch
import nltk
from nltk.tokenize import sent_tokenize
from nltk.corpus import stopwords
import random

# Seed setting for reproducibility
random_seed = 42
random.seed(random_seed)
np.random.seed(random_seed)
torch.manual_seed(random_seed)
torch.cuda.manual_seed_all(random_seed)

# Download necessary NLTK data and load models
nltk.download('punkt')
nltk.download('stopwords')
stanza.download('en')

# loading the SciBERT tokenizer and model, it may take a while
tokenizer = AutoTokenizer.from_pretrained("allenai/scibert_scivocab_uncased")
model = AutoModel.from_pretrained("allenai/scibert_scivocab_uncased")

# loading the stanza pipeline for POS tagging
nlp = stanza.Pipeline(lang='en', processors='tokenize,pos')
stop_words = set(stopwords.words('english'))

# this function splits the abstract into sentences level
def split_into_sentences(text):
    """
    Split the given text into sentences.

    Args:
    text (str): The text to split.

    Returns:
    list: A list of sentences.
    """
    sentences = sent_tokenize(text.lower())  # Convert text to lowercase
    return sentences


# this function gets the BERT embeddings for the sentence and the tokenized input
def get_bert_embeddings_and_tokens(sentence):
    """
    Get BERT embeddings for the sentence and the tokenized input.

    Args:
    sentence (str): The sentence to embed.

    Returns:
    tuple: A tuple containing tokenized inputs and their embeddings.
    """
    # Tokenize the sentence, max_length=512 is the maximum length of the sequence, and padding and truncation are set to True
    # to ensure that all the sequences have the same length
    inputs = tokenizer(sentence, return_tensors='pt', padding=True, truncation=True, max_length=512)
    outputs = model(**inputs)
    embeddings = outputs.last_hidden_state
    return inputs, embeddings


def get_pos_tags(sentence):
    """
    Get POS tags for each word in the sentence.

    Args:
    sentence (str): The sentence to get POS tags for.

    Returns:
    list: A list of tuples with word and its POS tag.
    """
    doc = nlp(sentence)
    return [(word.text, word.upos) for sent in doc.sentences for word in sent.words]


def is_pos_tag(pos_tag, tags=None):
    """
    Check if the POS tag is in the list of tags provided by the user or defaults to ['NOUN', 'ADJ'].
    You can find the list of POS tags here: https://universaldependencies.org/docs/u/pos/

    Args:
    pos_tag (str): The POS tag to check.
    tags (list, optional): The list of tags to check against. Defaults to ['NOUN', 'ADJ'].

    Returns:
    bool: True if pos_tag is in tags, False otherwise.
    """
    if tags is None:
        tags = ['NOUN', 'ADJ']
    return pos_tag in tags

def store_data(doc_id, sentences):
    """
    Store the processed data in a list of dictionaries.

    Args:
    doc_id (int): The document identifier.
    sentences (list): The list of sentences.

    Returns:
    list: A list of dictionaries with embedding data.
    """
    data = []

    for sentence_id, sentence in enumerate(sentences):
        inputs, embeddings = get_bert_embeddings_and_tokens(sentence)
        pos_tags = get_pos_tags(sentence)

        # Filter out stop words after POS tagging
        filtered_words = [(word, pos) for word, pos in pos_tags if word.lower() not in stop_words and is_pos_tag(pos)]

        # actually we don't need the pos tags, because in the previous step we have already filtered out the words that are not nouns or adjectives
        for word, _ in filtered_words:
            word_embeddings = []
            word_subwords = []
            word_lower = word.lower()
            tokenized_word = tokenizer.tokenize(word_lower)
            token_ids = tokenizer.convert_tokens_to_ids(tokenized_word)

            for token_id in token_ids:

                # Get the embedding for the token
                # In this stage, it can check the embedding is related to which sentence and which word! it extracts word ids!
                # As we feed in each step a sentence to the model, the model will return the embedding for each token in the sentence
                # for batch and text level the code should be changed!!!

                idx = (inputs['input_ids'] == token_id).nonzero(as_tuple=True)
                embedding = embeddings[idx].view(-1).tolist()

                word_embeddings.append(embedding)
                # we already have the token ids and embeddings, so we can convert the token ids to words(tokens)
                word_subwords.append(tokenizer.convert_ids_to_tokens(token_id))

            # This is an if statement to handle the possible error of empty word embeddings
            if word_embeddings:
                entry = {
                    'Doc ID': doc_id,
                    'Word': word,
                    'Sentence ID': sentence_id,
                    'Subword': word_subwords,
                    'Embeddings': word_embeddings
                }
                data.append(entry)
    return data

def process_documents(chunk_data):
    """
    Process a chunk of documents to extract embeddings.

    Args:
    chunk_data (list): A list of tuples, each containing a document ID and its corresponding text.

    Returns:
    list: A list of processed data entries for each word in the documents.
    """
    processed_data = []
    for doc_id, abstract in chunk_data:
        try:
            # Split the abstract into sentences, means sentence level
            sentences = split_into_sentences(abstract)
            data = store_data(doc_id, sentences)
            processed_data.extend(data)
        except Exception as e:
            print(f'Error processing document {doc_id}: {e}. Skipping...')
    return processed_data