load_data = function() {
  # All data are located in data directory. Load the csv files.
  train_data = readr::read_csv('data/train.csv')
  test_data = readr::read_csv('data/test.csv')
  full_data = dplyr::bind_rows(train_data, test_data, .id = 'train_test')
  return(full_data)
}
