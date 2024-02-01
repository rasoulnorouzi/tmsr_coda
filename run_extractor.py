# importing pandas for reading the json file and writing the output to a feather file
# importing process_documents from embedding_extractor.py to process the documents and get the embeddings
# %%
import numpy as np
import pandas as pd
from embedding_extractor import process_documents
from preprocessing import preprocess_text
import random
# set the seed for reproducibility
random.seed(8642)

# this is the path to the json file containing the abstracts and the path to the output and name of the feather file
input_file = '1_AllAbstracts.json'
output_file = 'embedding_vectors.feather'

# main function to read the json file, clean the data, process the documents and write the output to a feather file
def main(input_file, output_file):
    # Load and clean the dataset
    abstract_data = pd.read_json(input_file)

    # Remove rows with missing abstracts and reset the index
    # abstract_data_clean = abstract_data[abstract_data['abstract'] != 'nan'].reset_index(drop=True)
    abstract_data_clean = preprocess_text(abstract_data)
    # Get the number of documents in the dataset, this will be used for tracking progress
    len_doc = len(abstract_data_clean)

    # Process each document in the dataset and save in the processed_data list
    processed_data = []
    
    # loop over the documents and process them
    for doc_id, abstract in abstract_data_clean.iterrows():
        # Print progress
        print(f'Processing document {doc_id + 1} of {len_doc}')
        # Ensure to pass the abstract text, not the entire row
        chunk_data = [(abstract['id'], abstract['abstract'])] 
        processed_data.extend(process_documents(chunk_data))

    # Convert the processed data to a DataFrame
    processed_df = pd.DataFrame(processed_data)

    # convert the embeddings from a list to a numpy array
    processed_df["Embeddings"] = processed_df["Embeddings"].apply(lambda x: np.array(x))

    # Average the embeddings in axis 0
    processed_df["Embeddings"] = processed_df["Embeddings"].apply(lambda x: np.mean(x, axis=0))
    
    # reset the index
    processed_df = processed_df.reset_index(drop=True)
    # Export the DataFrame to Feather format
    processed_df.to_feather(output_file)
    print(f'Processed data exported to {output_file}')
    print("Done!")
    return processed_df

if __name__ == '__main__':
    processed_df = main(input_file, output_file)
# %%
