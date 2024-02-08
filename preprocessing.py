import re
import wordninja
import unicodedata
from langdetect import detect
import nltk
from nltk.tokenize import sent_tokenize
from langdetect import DetectorFactory
DetectorFactory.seed = 642

# Download necessary packages from nltk for text processing
nltk.download('punkt')

def is_nan_sample(text, threshold=200):
    """
    Check if the text is 'NaN' or has length less than a specified threshold.
    
    Parameters:
    text (str): The text to be checked.
    threshold (int): The minimum length threshold for the text. Defaults to 200.

    Returns:
    bool: True if text is 'NaN' or shorter than threshold, False otherwise.
    """
    return len(text) < threshold

def is_string_formatted_as_list(abstract):
    """
    Check if the given string is formatted as a list.
    
    Parameters:
    abstract (str): The string to be checked.

    Returns:
    bool: True if the string is formatted as a list, False otherwise.
    """
    abstract = abstract.strip()  # Remove leading and trailing whitespace
    return abstract.startswith('[') and abstract.endswith(']')

def is_not_english(text):
    """
    Check if the given text is not in English.
    
    Parameters:
    text (str): The text to be checked.

    Returns:
    bool: True if the text is not in English, False otherwise.
    """
    try:
        return detect(text) != "en"
    except:
        return True

def valid_abstracts(df):
    """
    Filter the dataframe to include only valid abstracts.

    This function removes rows where abstracts are NaN, formatted as lists,
    or not in English.

    Parameters:
    df (pandas.DataFrame): The dataframe containing the abstracts.

    Returns:
    pandas.DataFrame: A dataframe containing only valid abstracts.
    """
    df = df[df["abstract"].map(is_nan_sample) == False].reset_index(drop=True)
    df = df[df["abstract"].map(is_string_formatted_as_list) == False].reset_index(drop=True)
    df = df[df["abstract"].map(is_not_english) == False].reset_index(drop=True)
    return df

def remove_html_tags(text):
    """
    Remove HTML tags from the given text.

    Parameters:
    text (str): The text from which HTML tags are to be removed.

    Returns:
    str: The text with HTML tags removed.
    """
    html_tag_regex = re.compile(r'<[^>]+>')  # Regular expression for finding HTML tags
    return html_tag_regex.sub('', text)      # Replace HTML tags with an empty string

def replace_spaces(text):
    """
    Replace extra spaces in the text with a single space.

    Parameters:
    text (str): The text in which spaces are to be normalized.

    Returns:
    str: The text with extra spaces replaced.
    """
    pattern = r'\s{2,}'  # Regex pattern to match two or more space characters
    return re.sub(pattern, ' ', text)  # Replace matched spaces with a single space

def remove_initials(text):
    """
    Remove initials from the text.

    Parameters:
    text (str): The text from which initials are to be removed.

    Returns:
    str: The text with initials removed.
    """
    pattern = r'(\b\w\.\s?){1,}'  # Regex pattern to match initials
    return re.sub(pattern, '', text)  # Substitute found patterns with an empty string

def replace_ligatures(text):
    """
    Replace non-alphanumeric characters in the text with their ASCII equivalents.

    Parameters:
    text (str): The text in which ligatures are to be replaced.

    Returns:
    str: The text with ligatures replaced.
    """
    for char in text:
        if ord(char) > 127:  # Check for non-ASCII characters
            text = text.replace(char, unicodedata.normalize('NFKD', char).encode('ascii', 'ignore').decode('utf-8', 'ignore'))
    return text

def remove_email_addresses(text):
    """
    Remove email addresses from the text.

    Parameters:
    text (str): The text from which email addresses are to be removed.

    Returns:
    str: The text with email addresses removed.
    """
    email_pattern = re.compile(r'[a-zA-Z0-9+._-]+@[a-zA-Z0-9._-]+\.[a-zA-Z0-9_-]+')
    return email_pattern.sub('', text)

def remove_control_characters(text):
    """
    Remove control characters from the text.

    Parameters:
    text (str): The text from which control characters are to be removed.

    Returns:
    str: The text with control characters removed.
    """
    control_char_regex = re.compile(r'[\x00-\x1F\x7F]')
    return control_char_regex.sub('', text)

def is_concatenated_text(text, threshold=15):
    """
    Check if a text contains unusually long words, indicating concatenated text.
    
    Parameters:
    text (str): The text to be checked.
    threshold (int): The length threshold for a word to be considered unusually long.

    Returns:
    bool: True if text contains unusually long words, False otherwise.
    """
    return any(len(word) > threshold for word in text.split())

def split_concatenated_text_in_sentences(text):
    """
    Split concatenated text into individual words using wordninja, processing it sentence by sentence.

    Parameters:
    text (str): The concatenated text to be processed.

    Returns:
    str: The processed text with concatenated words split.
    """
    sentences = sent_tokenize(text)
    processed_sentences = []
    for sentence in sentences:
        if is_concatenated_text(sentence):
            sentence = ' '.join(wordninja.split(sentence))
        processed_sentences.append(sentence)
    return ' '.join(processed_sentences)

def remove_punctuation(text):
    """
    Remove punctuation from the text.

    Parameters:
    text (str): The text from which punctuation is to be removed.

    Returns:
    str: The text with punctuation removed.
    """
    pattern = re.compile(r"[`“”’’'\"]")  # Define a regex pattern for punctuation marks
    return re.sub(pattern, "", text)  # Replace the punctuation marks

def remove_numbers(text):
    """
    Remove numbers from the text.

    Parameters:
    text (str): The text from which numbers are to be removed.

    Returns:
    str: The text with numbers removed.
    """
    pattern = re.compile(r"\d+")  # Define a regex pattern for numbers
    return re.sub(pattern, "", text)  # Replace the numbers

def preprocess_text(df):
    """
    Perform a series of preprocessing steps on a dataframe's 'abstract' column.

    Steps include: removing invalid abstracts, HTML tags, email addresses, control characters,
    ligatures, initials, punctuation, splitting concatenated text, and normalizing spaces.

    Parameters:
    df (pandas.DataFrame): The dataframe to be preprocessed.

    Returns:
    pandas.DataFrame: The preprocessed dataframe.
    """
    print("Removing invalid abstracts...")

    df_valid = valid_abstracts(df)

    print("Starting preprocessing...")

    processing_steps = [
        remove_html_tags,
        remove_email_addresses,
        remove_control_characters,
        replace_ligatures,
        remove_initials,
        remove_punctuation,
        remove_numbers,
        split_concatenated_text_in_sentences,
        replace_spaces,
        lambda x: x.strip()
    ]

    for step in processing_steps:
        df_valid["abstract"] = df_valid["abstract"].map(step)
    df_valid = df_valid.reset_index(drop=True)

    print("Preprocessing complete!")

    return df_valid
